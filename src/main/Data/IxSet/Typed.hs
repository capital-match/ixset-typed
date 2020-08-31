{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- |
An efficient implementation of queryable sets.

Assume you have a family of types such as:

> data Entry      = Entry Author [Author] Updated Id Content
>   deriving (Show, Eq, Ord)
> newtype Updated = Updated UTCTime
>   deriving (Show, Eq, Ord)
> newtype Id      = Id Int64
>   deriving (Show, Eq, Ord)
> newtype Content = Content String
>   deriving (Show, Eq, Ord)
> newtype Author  = Author Email
>   deriving (Show, Eq, Ord)
> type Email      = String
> data Test = Test
>   deriving (Show, Eq, Ord)

1. Decide what parts of your type you want indexed and make 'Indexed'
instances:

    > type EntryIxs = '[Author, Id, Updated, Test]
    > type IxEntry  = IxSet EntryIxs Entry
    >
    > instance Indexed Entry Author where
    >   ixFun (Entry a b _ _ _) = a : b
    >
    > instance Indexed Entry Id where
    >   ixFun (Entry _ _ _ i _) = [i]
    >
    > instance Indexed Entry Updated where
    >   ixFun (Entry _ _ u _ _) = [u]
    >
    > instance Indexed Entry Test where
    >   ixFun _ = []                    -- bogus index

2. Use 'insert', 'insertList', 'delete', 'updateIx', 'deleteIx'
and 'empty' to build up an 'IxSet' collection:

    > entries  = insertList [e1, e2, e3, e4] (empty :: IxEntry)
    > entries1 = foldr delete entries [e1, e3]
    > entries2 = updateIx (Id 4) e5 entries

3. Use the query functions below to grab data from it:

    > entries @= Author "john@doe.com" @< Updated t1

    Statement above will find all items in entries updated earlier than
    @t1@ by @john\@doe.com@.

4. Text index

    If you want to do add a text index create a calculated index.  Then if you want
    all entries with either @word1@ or @word2@, you change the instance
    to:

    > newtype Word = Word String
    >   deriving (Show, Eq, Ord)
    >
    > type EntryIxs = '[..., Word]
    > instance Indexed Entry Word where
    >     ixFun (Entry _ _ _ _ (Content s)) = map Word (words s)

    Now you can do this query to find entries with any of the words:

    > entries @+ [Word "word1", Word "word2"]

    And if you want all entries with both:

    > entries @* [Word "word1", Word "word2"]

5. Find only the first author

    If an @Entry@ has multiple authors and you want to be able to query on
    the first author only, define a @FirstAuthor@ datatype and create an
    index with this type.  Now you can do:

    > newtype FirstAuthor = FirstAuthor Email
    >   deriving (Show, Eq, Ord)
    >
    > type EntryIxs = '[..., FirstAuthor]
    > instance Indexed Entry FirstAuthor where
    >     ixFun (Entry author _ _ _ _) = [FirstAuthor author]

    > entries @= (FirstAuthor "john@doe.com")  -- guess what this does

-}

module Data.IxSet.Typed
    (
     -- * Set type
     IxSet(),
     IxList(),
     Indexable,
     IsIndexOf(),
     All,
     -- ** Declaring indices
     Ix(),
     Indexed(..),
     MkEmptyIxList(),

     -- ** Exceptions
     NotUniqueException(..),

     -- * Changes to set
     insert,
     insertList,
     delete,
     updateIx,
     updateUnique,
     deleteIx,
     Container.Polymorphic(..),
     IxContainer,
     deleteUnique,
     alterIx,
     filter,

     -- * Creation
     empty,
     fromSet,
     fromList,

     -- * Conversion
     toSet,
     toList,
     toAscList,
     toDescList,
     toFullDescList,
     getOne,
     getOneOr,
     getUnique,

     -- * Size checking
     size,
     null,

     -- * Set operations
     (&&&),
     (|||),
     union,
     intersection,
     difference,

     -- * Indexing
     (@=),
     (@<),
     (@>),
     (@<=),
     (@>=),
     (@><),
     (@>=<),
     (@><=),
     (@>=<=),
     (@+),
     (@*),
     getEQ,
     lookup,
     member,
     getLT,
     getGT,
     getLTE,
     getGTE,
     getRange,
     groupBy,
     groupAscBy,
     groupDescBy,
     groupFullDescBy,
     indexKeys,

     -- * Joins
     Joined(..),

     -- * Debugging and optimization
     forceIndexesPar,
     forceIndexesSeq,
     stats
)
where

import Prelude hiding (filter, null, lookup)

import Control.Arrow (first, second)
import Control.DeepSeq
import Control.Monad.Catch
import Data.Either
import qualified Data.Foldable as Fold
import Data.IxSet.Typed.Ix (Indexed(..),IndexType,Ix (Ix))
import Data.IxSet.Typed.Container (OrderedContainer, IxContainer, Container)
import qualified Data.IxSet.Typed.Container as Container
import qualified Data.IxSet.Typed.Ix as Ix
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Coerce
import Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Parallel.Strategies
import Data.Typeable
import GHC.Exts (Constraint)

--------------------------------------------------------------------------
-- The main 'IxSet' datatype.
--------------------------------------------------------------------------

-- | Set with associated indices.
--
-- The type-level list 'ixs' contains all types that are valid index keys.
-- The type 'a' is the type of elements in the indexed set.
--
-- On strictness: An 'IxSet' is "mostly" spine-strict. It is generally
-- spine-strict in the set itself. All operations on 'IxSet' with the
-- exception of queries are spine-strict in the indices as well. Query
-- operations, however, are lazy in the indices, so querying a number of
-- times and subsequently selecting the result will not unnecessarily
-- rebuild all indices.
--
data IxSet (ixs :: [*]) (a :: *) where
  IxSet :: !(Set a) -> !(IxList ixs a) -> IxSet ixs a

-- | An 'IxList' is a list of actual indices for an 'IxSet'. It is represented
-- as a list of maps of sets.
data IxList (ixs :: [*]) (a :: *) where
  Nil   :: IxList '[] a
  (:::) :: Ix ix a -> IxList ixs a -> IxList (ix ': ixs) a

infixr 5 :::

-- | A strict variant of ':::'.
(!:::) :: Ix ix a -> IxList ixs a -> IxList (ix ': ixs) a
(!:::) !ix !ixs = ix ::: ixs

infixr 5 !:::


--------------------------------------------------------------------------
-- Type-level tools for dealing with indexed sets.
--
-- These are partially internal. TODO: Move to different module?
--------------------------------------------------------------------------


-- | The constraint @All c xs@ says the @c@ has to hold for all
-- elements in the type-level list @xs@.
--
-- Example:
--
-- > All Ord '[Int, Char, Bool]
--
-- is equivalent to
--
-- > (Ord Int, Ord Char, Ord Bool)
--
type family All (c :: * -> Constraint) (xs :: [*]) :: Constraint
type instance All c '[]       = ()
type instance All c (x ': xs) = (c x, All c xs)

type family AllIxType a (c :: * -> Constraint) (xs :: [*]) :: Constraint
type instance AllIxType a c '[]       = ()
type instance AllIxType a c (x ': xs) = (c (IndexType a x), AllIxType a c xs)


-- | 'Indexable' is a convenient shorthand for the instances that are necessary
-- to use 'IxSet'. If you want to use @'IxSet' ixs a@, you need
--
-- * An 'Ord' instance for @a@
-- * An 'Ord' instance for each @ix@ in @ixs@
-- * An 'Indexed' instance for @a@ and each @ix@ in @ixs@
type Indexable ixs a = (AllIxType a IxContainer ixs , AllIxType a Ord ixs, All (Indexed a) ixs, Ord a, MkEmptyIxList a ixs)


-- | Operations related to the type-level list of index types.
class (IxContainer (IndexType a ix)) => IsIndexOf a (ix :: *) (ixs :: [*]) where

  -- | Provide access to the selected index in the list.
  access :: IxList ixs a -> Ix ix a

  -- | Container over the index list, treating the selected different
  -- from the rest.
  --
  -- The function 'mapAt' is lazy in the index list structure,
  -- because it is used by query operations.
  mapAt :: (AllIxType a IxContainer ixs ,AllIxType a Ord ixs, All (Indexed a) ixs)
        => (Indexed a ix => Ix ix a -> Ix ix a)
              -- ^ what to do with the selected index
        -> (forall ix'. (IxContainer (IndexType a ix'), Indexed a ix') => Ix ix' a -> Ix ix' a)
              -- ^ what to do with the other indices
        -> IxList ixs a -> IxList ixs a

instance
  {-# OVERLAPPING #-}
  (IxContainer (IndexType a ix)) => IsIndexOf a ix (ix ': ixs) where
  access (x ::: _xs)     = x
  mapAt fh ft (x ::: xs) = fh x ::: mapIxList ft xs

instance
  {-# OVERLAPPABLE #-}
  IsIndexOf a ix ixs => IsIndexOf a ix (ix' ': ixs) where
  access (_x ::: xs)     = access xs
  mapAt fh ft (x ::: xs) = ft x ::: mapAt @a @ix @ixs fh ft xs

-- | Return the length of an index list.
--
-- TODO: Could be statically unrolled.
lengthIxList :: forall ixs a. IxList ixs a -> Int
lengthIxList = go 0
  where
    go :: forall ixs'. Int -> IxList ixs' a -> Int
    go !acc Nil        = acc
    go !acc (_ ::: xs) = go (acc + 1) xs
{-# INLINE lengthIxList #-}

-- | Turn an index list into a normal list, given a function that
-- turns an arbitrary index into an element of a fixed type @r@.
ixListToList :: (AllIxType a IxContainer ixs, AllIxType a Ord ixs)
             => (forall ix. (IxContainer (IndexType a ix)) => Ix ix a -> r)
                  -- ^ what to do with each index
             -> IxList ixs a -> [r]
ixListToList _ Nil        = []
ixListToList f (x ::: xs) = f x : ixListToList f xs

-- | Container over an index list.
mapIxList :: (AllIxType a IxContainer  ixs , AllIxType a Ord ixs, All (Indexed a) ixs)
          => (forall ix. (IxContainer (IndexType a ix), Indexed a ix) => Ix ix a -> Ix ix a)
                -- ^ what to do with each index
          -> IxList ixs a -> IxList ixs a
mapIxList _ Nil        = Nil
mapIxList f (x ::: xs) = f x ::: mapIxList f xs
{-# INLINE mapIxList #-}

-- | Container over an index list (spine-strict).
mapIxListPar' :: (AllIxType a Ord ixs, All (Indexed a) ixs)
           =>
                 -- ^ what to do with each index
           IxList ixs a -> Eval (IxList ixs a)
mapIxListPar'  Nil        = return Nil
mapIxListPar'  (x ::: xs) = do
  x' <- rparWith rseq x
  xs' <- mapIxListPar' xs
  return (x' ::: xs')
{-# INLINE mapIxListPar' #-}

-- | Container over an index list (spine-strict).
mapIxList' :: (AllIxType a IxContainer ixs, AllIxType a Ord ixs, All (Indexed a) ixs)
           => (forall ix. (IxContainer (IndexType a ix), Indexed a ix) => Ix ix a -> Ix ix a)
                 -- ^ what to do with each index
           -> IxList ixs a -> IxList ixs a
mapIxList' _ Nil        = Nil
mapIxList' f (x ::: xs) = f x !::: mapIxList' f xs
{-# INLINE mapIxList' #-}


-- | Zip two index lists of compatible type (spine-strict).
zipWithIxList' :: (AllIxType a IxContainer ixs, AllIxType a Ord ixs, All (Indexed a) ixs)
               => (forall ix. (IxContainer (IndexType a ix) , Indexed a ix) => Ix ix a -> Ix ix a -> Ix ix a)
                    -- ^ how to combine two corresponding indices
               -> IxList ixs a -> IxList ixs a -> IxList ixs a
zipWithIxList' _ Nil        Nil        = Nil
zipWithIxList' f (x ::: xs) (y ::: ys) = f x y !::: zipWithIxList' f xs ys
{-# INLINE zipWithIxList' #-}

--------------------------------------------------------------------------
-- Various instances for 'IxSet'
--------------------------------------------------------------------------

instance Indexable ixs a => Eq (IxSet ixs a) where
  IxSet a _ == IxSet b _ = a == b

instance Indexable ixs a => Ord (IxSet ixs a) where
  compare (IxSet a _) (IxSet b _) = compare a b

instance (Indexable ixs a, Show a) => Show (IxSet ixs a) where
  showsPrec prec = showsPrec prec . toSet

instance (Indexable ixs a, Read a) => Read (IxSet ixs a) where
  readsPrec n = map (first fromSet) . readsPrec n

instance (Typeable ixs, Typeable a, Indexable ixs a, SafeCopy a) => SafeCopy (IxSet ixs a) where
  putCopy = contain . safePut . toList
  getCopy = contain $ fmap fromList safeGet
 
instance (AllIxType a IxContainer ixs ,  All NFData ixs, NFData a) => NFData (IxList ixs a) where
  rnf Nil        = ()
  rnf (Ix x ::: xs) = rnf1 x `seq` rnf xs

instance (AllIxType a IxContainer ixs, All NFData ixs, NFData a) => NFData (IxSet ixs a) where
  rnf (IxSet a ixs) = rnf a `seq` rnf ixs

instance Indexable ixs a => Semigroup (IxSet ixs a) where
  (<>) = mappend

instance Indexable ixs a => Monoid (IxSet ixs a) where
  mempty  = empty
  mappend = union

instance Foldable (IxSet ixs) where
  fold      = Fold.fold      . toSet
  foldMap f = Fold.foldMap f . toSet
  foldr f z = Fold.foldr f z . toSet
  foldl f z = Fold.foldl f z . toSet
  foldr' f z = Fold.foldr' f z . toSet
  foldl' f z = Fold.foldl' f z . toSet
  elem a    = Fold.elem a    . toSet -- Not recommended.
  minimum   = Fold.minimum   . toSet
  maximum   = Fold.maximum   . toSet
  sum       = Fold.sum       . toSet
  product   = Fold.product   . toSet
  length    = size
  toList    = toList
  null      = null

-- | Thrown when a function expecting a single unique value encounters
-- multiple values

data NotUniqueException = NotUnique
  deriving (Show)

instance Exception NotUniqueException

--------------------------------------------------------------------------
-- 'IxSet' construction
--------------------------------------------------------------------------

-- | An empty 'IxSet'.
empty :: Indexable ixs a => IxSet ixs a
empty = IxSet Set.empty mkEmptyIxList

-- | Create an empty 'IxList' which is part of an empty 'IxSet'. This class is
-- used internally because instances provide a way to do case analysis on a
-- type-level list. If you see an error message about this constraint not being
-- satisfied, make sure the @ixs@ argument to 'Indexable' is a type-level list.
class MkEmptyIxList a (ixs :: [*]) where
  mkEmptyIxList :: IxList ixs a
instance MkEmptyIxList a '[] where
  mkEmptyIxList = Nil
instance (IxContainer (IndexType a ix) , MkEmptyIxList a ixs) => MkEmptyIxList a (ix ': ixs) where
  mkEmptyIxList = (Ix Container.empty) ::: mkEmptyIxList

-- | An 'Indexed' class asserts that it is possible to extract indices of type
-- @ix@ from a type @a@. Provided function should return a list of indices where
-- the value should be found. There are no predefined instances for 'Indexed'.
--------------------------------------------------------------------------
-- Modification of 'IxSet's
--------------------------------------------------------------------------

type SetOp =
    forall a. Ord a => a -> Set a -> Set a

type IndexOp =
    forall k a. (IxContainer k,Ord a) => k -> a -> Container k (Set a) -> Container k (Set a)

-- | Higher order operator for modifying 'IxSet's.  Use this when your
-- final function should have the form @a -> 'IxSet' a -> 'IxSet' a@,
-- e.g. 'insert' or 'delete'.
change :: forall ixs a. Indexable ixs a
       => SetOp -> IndexOp -> a -> IxSet ixs a -> IxSet ixs a
change !opS !opI x (IxSet a indexes) = IxSet (opS x a) v
  where
    v :: IxList ixs a
    v = mapIxList' update indexes

    update :: forall ix. (IxContainer (IndexType a ix), Indexed a ix) => Ix ix a -> Ix ix a
    update (Ix index) = Ix index'
      where
        ii :: forall k . (IxContainer k) => Container k (Set a) -> k -> Container k (Set a)
        ii m dkey = opI dkey x m
        index' :: Container (IndexType a ix) (Set a)
        index' = List.foldl' ii index (ixFunRep @a @ix x) 
{-# INLINE change #-}

insertList :: forall ixs a. (AllIxType a IxContainer ixs , AllIxType a Ord ixs, All (Indexed a) ixs, Ord a)
           => [a] -> IxSet ixs a -> IxSet ixs a
insertList xs (IxSet a indexes) = IxSet (List.foldl' (\ b x -> Set.insert x b) a xs) ( mapIxList' update indexes)
  where

    update :: forall ix. (IxContainer (IndexType a ix) ,Indexed a ix) => Ix ix a -> Ix ix a
    update (Ix index) = Ix index'
      where
        index' :: Container (IndexType a ix) (Set a)
        index' = List.foldl' (\m v -> List.foldl' (\m' k-> Ix.insert k v m') m (ixFunRep @a @ix v)) index xs

-- | Internal helper function that takes a partial index from one index
-- set and rebuilds the rest of the structure of the index set.
--
-- Slightly rewritten comment from original version regarding dss / index':
--
-- We try to be really clever here. The partialindex is a Container of Sets
-- from original index. We want to reuse it as much as possible. If there
-- was a guarantee that each element is present at at most one key we
-- could reuse originalindex as it is. But there can be more, so we need to
-- add remaining ones (in updateh). Anyway we try to reuse old structure and
-- keep new allocations low as much as possible.
fromContainerOfSet :: forall ixs ix a. (Eq (IndexType a ix), Indexable ixs a, IsIndexOf a ix ixs)
              => ix -> Set a -> IxSet ixs a
fromContainerOfSet v a =
    IxSet a (mapAt @a @ix @ixs updateh updatet mkEmptyIxList)
  where

    -- Update function for the index corresponding to partialindex.
    updateh :: (Indexed a ix) => Ix ix a -> Ix ix a
    updateh (Ix _) = Ix ix
      where
        dss :: [(IndexType a ix, a)]
        dss = do 
            x <- Set.toList a
            let reps = ixFunRep @a @ix x
            if length reps > 1
              then  do 
                k <- reps 
                if (k /=  ixRep @a @ix v) 
                then return (k,x)
                else [] 
            else []


        ix :: Container (IndexType a ix) (Set a)
        ix = Ix.insertList dss (Container.singleton (ixRep @a @ix v) a)



    -- Update function for all other indices.
    updatet :: forall ix'. (IxContainer (IndexType a ix'), Indexed a ix') => Ix ix' a -> Ix ix' a
    updatet (Ix _) = Ix ix
      where
        ix :: Container (IndexType a ix') (Set a)
        ix = List.foldl' (\m v' -> List.foldl' (\m' k-> Ix.insert k v' m') m (ixFunRep @a @ix' v')) Container.empty (Set.toList a)
{-# INLINE fromContainerOfSet #-}


fromContainerOfSets :: forall ixs ix a. (Indexable ixs a, IsIndexOf a ix ixs)
              => Container (IndexType a ix) (Set a) -> IxSet ixs a
fromContainerOfSets partialindex =
    IxSet a (mapAt @a @ix @ixs updateh updatet mkEmptyIxList)
  where
    a :: Set a
    a = nonEmptyFoldl (Container.elems partialindex)
    nonEmptyFoldl (i:xs) = List.foldl' Set.union i xs
    nonEmptyFoldl  [] = Set.empty


    -- Update function for the index corresponding to partialindex.
    updateh :: Indexed a ix => Ix ix a -> Ix ix a
    updateh (Ix _) = Ix ix
      where
        dss :: [(IndexType a ix, a)]
        dss = [(k, x) | x <- Set.toList a, k <- ixFunRep @a @ix x, not (Container.member k partialindex)]

        ix :: Container (IndexType a ix) (Set a)
        ix = Ix.insertList dss partialindex

    -- Update function for all other indices.
    updatet :: forall ix'. (IxContainer (IndexType a ix'), Indexed a ix' ) => Ix ix' a -> Ix ix' a
    updatet (Ix _) = Ix ix
      where
        dss :: [(IndexType a ix', a)]
        dss = [(k, x) | x <- Set.toList a, k <- ixFunRep @a @ix' x]

        ix :: Container (IndexType a ix') (Set a)
        ix = Ix.fromList dss
{-# INLINE fromContainerOfSets #-}

-- | Inserts an item into the 'IxSet'. If your data happens to have a primary
-- key this function is most likely /not/ what you want. In this case, use
-- 'updateIx' instead.
insert :: Indexable ixs a => a -> IxSet ixs a -> IxSet ixs a
insert = change Set.insert Ix.insert
{-# INLINE insert #-}

-- | Removes an item from the 'IxSet'.
delete :: Indexable ixs a => a -> IxSet ixs a -> IxSet ixs a
delete = change Set.delete Ix.delete
{-# INLINE delete #-}

-- | Internal implementation for update* family
updateIx' :: (Eq (IndexType a ix), Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs, MonadThrow m)
         => (IxSet ixs a -> m (Maybe a)) -> ix -> a -> IxSet ixs a -> m (IxSet ixs a)
updateIx' get i new ixset = do
  existing <- get $ ixset @= i
  pure $ insert new $
    maybe ixset (flip delete ixset) $
    existing
{-# INLINE updateIx' #-}

-- | Internal implementation for delete* family
deleteIx' :: (Eq (IndexType a ix), Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs, MonadThrow m)
         => (IxSet ixs a -> m (Maybe a)) -> ix -> IxSet ixs a -> m (IxSet ixs a)
deleteIx' get i ixset = do
  existing <- get $ ixset @= i
  pure $ maybe ixset (flip delete ixset) $
    existing
{-# INLINE deleteIx' #-}

-- | Will replace the item with the given index of type 'ix'.
-- Only works if there is at most one item with that index in the 'IxSet'.
-- Will not change 'IxSet' if you have more than one item with given index.
updateIx :: (Eq (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
         => ix -> a -> IxSet ixs a -> IxSet ixs a
updateIx i new ixset = fromRight ixset $ updateIx' (Right . getOne) i new ixset
{-# INLINE updateIx #-}


-- | Will replace the item with the given index of type 'ix'.
-- Only works if there is at most one item with that index in the 'IxSet'.
-- Will not change 'IxSet' if you have more than one item with given index.
alterIx :: (Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
         => ix -> (Maybe a -> Maybe a)  -> IxSet ixs a -> (IxSet ixs a, (Maybe a, Maybe a))
alterIx i f ixset =
  let existing = lookup i ixset
      new = f existing
  in ((maybe id insert new) $
    maybe ixset (flip delete ixset) $
    existing,(existing,new))
{-# INLINE alterIx #-}

-- | Will replace the item with the given index of type 'ix'.
-- Only works if there is at most one item with that index in the 'IxSet'.
-- Will throw if there is more than one item with given index.

-- Throws: 'NotUniqueException

updateUnique :: (Eq (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs, MonadThrow m)
         => ix -> a -> IxSet ixs a -> m (IxSet ixs a)
updateUnique = updateIx' getUnique

-- | Will delete the item with the given index of type 'ix'.
-- Only works if there is at  most one item with that index in the 'IxSet'.
-- Will not change 'IxSet' if you have more than one item with given index.
deleteIx :: (Eq (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
         => ix -> IxSet ixs a -> IxSet ixs a
deleteIx i ixset = fromRight ixset $ deleteIx' (Right . getOne) i ixset
{-# INLINE deleteIx #-}

-- | Will delete the item with the given index of type 'ix'.
-- Only works if there is at  most one item with that index in the 'IxSet'.
-- Will throw if there is more than one item with given index.

-- Throws: 'NotUniqueException

deleteUnique :: (Eq (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs, MonadThrow m)
         => ix -> IxSet ixs a -> m (IxSet ixs a)
deleteUnique = deleteIx' getUnique


-- | /O(n)/. Filter all elements that satisfy the predicate. In general, using
-- indexing operations is preferred, so instead of using 'filter' you should
-- construct an index that accomplishes this. This function will call the
-- provided predicate exactly once for each element in the 'IxSet'.
filter :: forall ixs a. Indexable ixs a => (a -> Bool) -> (IxSet ixs a -> IxSet ixs a)
filter f (IxSet a il) = IxSet a' il'
  where
    a' = Set.filter f a
    il' = mapIxList update il
      where
        update :: forall ix. IxContainer (IndexType a ix) => Ix ix a -> Ix ix a
        update (Ix ix) = Ix (Ix.filterFrom a' ix)

--------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------

-- | Converts an 'IxSet' to a 'Set' of its elements.
toSet :: IxSet ixs a -> Set a
toSet (IxSet a _) = a

-- | Converts a 'Set' to an 'IxSet'.
fromSet :: forall ixs a. (Indexable ixs a) => Set a -> IxSet ixs a
fromSet set = IxSet set v
  where
    v :: IxList ixs a
    v = mapIxList mkIx mkEmptyIxList

    mkIx :: forall ix. (IxContainer (IndexType a ix), Indexed a ix) => Ix ix a -> Ix ix a
    mkIx (Ix _) = Ix $ Ix.fromSet (ixFunRep @a @ix) set 
{-# INLINE fromSet #-}

-- | Converts a list to an 'IxSet'.
fromList :: (Indexable ixs a) => [a] -> IxSet ixs a
fromList list = insertList list empty
{-# INLINE fromList #-}

-- | Returns the number of unique items in the 'IxSet'.
size :: IxSet ixs a -> Int
size = Set.size . toSet

-- | Converts an 'IxSet' to its list of elements.
toList :: IxSet ixs a -> [a]
toList = Set.toList . toSet

-- | Converts an 'IxSet' to its list of elements.
--
-- List will be sorted in ascending order by the index 'ix'. When 'ix' values
-- are equivalent, the values will be sorted in ascending order by their 'Ord'
-- instance.
--
-- The list may contain duplicate entries if a single value produces multiple keys.
toAscList :: forall proxy ix ixs a. (OrderedContainer (IndexType a ix), Coercible ix (IndexType a ix) , IsIndexOf a ix ixs) => proxy ix -> IxSet ixs a -> [a]
toAscList _ ixset = concatMap snd (groupAscBy ixset :: [(ix, [a])])

-- | Converts an 'IxSet' to its list of elements.
--
-- List will be sorted in descending order by the index 'ix'. When 'ix' values
-- are equivalent, the values will be sorted in /ascending/ order by their 'Ord'
-- instance.
--
-- The list may contain duplicate entries if a single value produces multiple keys.
toDescList :: forall proxy ix ixs a. (OrderedContainer (IndexType a ix) , Coercible ix (IndexType a ix) , IsIndexOf a ix ixs) => proxy ix -> IxSet ixs a -> [a]
toDescList _ ixset = concatMap snd (groupDescBy ixset :: [(ix, [a])])

-- | Converts an 'IxSet' to its list of elements.
--
-- List will be sorted in descending order by the index 'ix'. When 'ix' values
-- are equivalent, the values will be sorted in descending order by their 'Ord'
-- instance.
--
-- The list may contain duplicate entries if a single value produces multiple keys.
toFullDescList :: forall proxy ix ixs a. (OrderedContainer (IndexType a ix),Coercible ix (IndexType a ix) , IsIndexOf a ix ixs) => proxy ix -> IxSet ixs a -> [a]
toFullDescList _ ixset = concatMap snd (groupFullDescBy ixset :: [(ix, [a])])

-- | If the 'IxSet' is a singleton it will return the one item stored in it.
-- If 'IxSet' is empty or has many elements this function returns 'Nothing'.
getOne :: IxSet ixs a -> Maybe a
getOne ixset = case toList ixset of
                   [x] -> Just x
                   _   -> Nothing

-- | Like 'getOne' with a user-provided default.
getOneOr :: a -> IxSet ixs a -> a
getOneOr def = fromMaybe def . getOne

-- | Like getOne, but error if multiple items exist

-- Throws: 'NotUniqueException
getUnique :: MonadThrow m => IxSet ixs a -> m (Maybe a)
getUnique ixset = case toList ixset of
                    [x] -> pure $ Just x
                    [] -> pure Nothing
                    _ -> throwM NotUnique

-- | Return 'True' if the 'IxSet' is empty, 'False' otherwise.
null :: IxSet ixs a -> Bool
null (IxSet a _) = Set.null a

--------------------------------------------------------------------------
-- Set operations
--------------------------------------------------------------------------

-- | An infix 'intersection' operation.
(&&&) :: Indexable ixs a => IxSet ixs a -> IxSet ixs a -> IxSet ixs a
(&&&) = intersection

-- | An infix 'union' operation.
(|||) :: Indexable ixs a => IxSet ixs a -> IxSet ixs a -> IxSet ixs a
(|||) = union

infixr 5 &&&
infixr 5 |||

-- | Takes the union of the two 'IxSet's.
union :: Indexable ixs a => IxSet ixs a -> IxSet ixs a -> IxSet ixs a
union (IxSet a1 x1) (IxSet a2 x2)
  | Set.null a1 = IxSet a2 x2
  | Set.null a2 = IxSet a1 x1
  | otherwise = IxSet (Set.union a1 a2)
    (zipWithIxList' (\ (Ix a) (Ix b) -> Ix (Ix.union a b)) x1 x2)
-- TODO: function is taken from the first

-- | Takes the intersection of the two 'IxSet's.
intersection :: Indexable ixs a => IxSet ixs a -> IxSet ixs a -> IxSet ixs a
intersection (IxSet a1 x1) (IxSet a2 x2) =
  IxSet (Set.intersection a1 a2)
    (zipWithIxList' (\ (Ix a) (Ix b) -> Ix (Ix.intersection a b)) x1 x2)

difference :: Indexable ixs a => IxSet ixs a -> IxSet ixs a -> IxSet ixs a
difference (IxSet a1 x1) (IxSet a2 x2) =
  IxSet (Set.difference a1 a2)
    (zipWithIxList' (\(Ix a) (Ix b) -> Ix (Ix.difference a b)) x1 x2)
-- TODO: function is taken from the first

--------------------------------------------------------------------------
-- Query operations
--------------------------------------------------------------------------

-- | Infix version of 'getEQ'.
(@=) :: (Eq (IndexType a ix), Indexable ixs a, Indexed a ix , IsIndexOf a ix ixs)
     => IxSet ixs a -> ix -> IxSet ixs a
ix @= v = getEQ v ix

-- | Infix version of 'getLT'.
(@<) :: (OrderedContainer (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
     => IxSet ixs a -> ix -> IxSet ixs a
ix @< v = getLT v ix

-- | Infix version of 'getGT'.
(@>) :: (OrderedContainer (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
     => IxSet ixs a -> ix -> IxSet ixs a
ix @> v = getGT v ix

-- | Infix version of 'getLTE'.
(@<=) :: (OrderedContainer (IndexType a ix), Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
      => IxSet ixs a -> ix -> IxSet ixs a
ix @<= v = getLTE v ix

-- | Infix version of 'getGTE'.
(@>=) :: (OrderedContainer (IndexType a ix), Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
      => IxSet ixs a -> ix -> IxSet ixs a
ix @>= v = getGTE v ix

-- | Returns the subset with indices in the open interval (k,k).
(@><) :: (OrderedContainer (IndexType a ix), Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
      => IxSet ixs a -> (ix, ix) -> IxSet ixs a
ix @>< (v1,v2) = getLT v2 $ getGT v1 ix

-- | Returns the subset with indices in [k,k).
(@>=<) :: (OrderedContainer (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
       => IxSet ixs a -> (ix, ix) -> IxSet ixs a
ix @>=< (v1,v2) = getLT v2 $ getGTE v1 ix

-- | Returns the subset with indices in (k,k].
(@><=) :: (OrderedContainer (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
       => IxSet ixs a -> (ix, ix) -> IxSet ixs a
ix @><= (v1,v2) = getLTE v2 $ getGT v1 ix

-- | Returns the subset with indices in [k,k].
(@>=<=) :: (OrderedContainer (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
        => IxSet ixs a -> (ix, ix) -> IxSet ixs a
ix @>=<= (v1,v2) = getLTE v2 $ getGTE v1 ix

-- | Creates the subset that has an index in the provided list.
(@+) :: (Eq (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
     => IxSet ixs a -> [ix] -> IxSet ixs a
ix @+ [e] = ix @= e
ix @+ list = List.foldl' union  empty $  map (ix @=) list

-- | Creates the subset that matches all the provided indices.
(@*) :: (Eq (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
     => IxSet ixs a -> [ix] -> IxSet ixs a
ix @* [e]= ix @=  e
ix @* list = List.foldl' intersection ix $ map (ix @=) list

-- | Returns the subset with an index equal to the provided key.  The
-- set must be indexed over key type, doing otherwise results in
-- a compile error.

-- | Returns the subset with an index less than the provided key.  The
-- set must be indexed over key type, doing otherwise results in
-- a compile error.
getLT :: (OrderedContainer (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
      => ix -> IxSet ixs a -> IxSet ixs a
getLT = getOrd2 True False False

-- | Returns the subset with an index greater than the provided key.
-- The set must be indexed over key type, doing otherwise results in
-- a compile error.
getGT :: (OrderedContainer (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
      => ix -> IxSet ixs a -> IxSet ixs a
getGT = getOrd2 False False True

-- | Returns the subset with an index less than or equal to the
-- provided key.  The set must be indexed over key type, doing
-- otherwise results in a compile error.
getLTE :: (OrderedContainer (IndexType a ix), Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
       => ix -> IxSet ixs a -> IxSet ixs a
getLTE = getOrd2 True True False

-- | Returns the subset with an index greater than or equal to the
-- provided key.  The set must be indexed over key type, doing
-- otherwise results in a compile error.
getGTE :: (OrderedContainer (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
       => ix -> IxSet ixs a -> IxSet ixs a
getGTE = getOrd2 False True True

-- | Returns the subset with an index within the interval provided.
-- The bottom of the interval is closed and the top is open,
-- i. e. [k1;k2).  The set must be indexed over key type, doing
-- otherwise results in a compile error.
getRange :: (OrderedContainer (IndexType a ix),Indexed a ix ,Indexable ixs a, IsIndexOf a ix ixs)
         => ix -> ix -> IxSet ixs a -> IxSet ixs a
getRange k1 k2 ixset = getGTE k1 (getLT k2 ixset)

-- | Returns lists of elements paired with the indices determined by
-- type inference.
groupBy :: forall ix ixs a. (Coercible ix (IndexType a ix) , IsIndexOf a ix ixs ) => IxSet ixs a -> [(ix, [a])]
groupBy (IxSet _ indexes) = f (access indexes)
  where
    f :: Ix ix a -> [(ix, [a])]
    f (Ix index) = map (coerce . second Set.toList) (Container.toList index)

-- | Returns the list of index keys being used for a particular index.
indexKeys :: forall ix ixs a . (Coercible ix (IndexType a ix) ,IxContainer (IndexType a ix), IsIndexOf a ix ixs) => IxSet ixs a -> [ix]
indexKeys (IxSet _ indexes) = f (access indexes)
  where
    f :: Ix ix a -> [ix]
    f (Ix index) = coerce $ Container.keys index

-- | Returns lists of elements paired with the indices determined by
-- type inference.
--
-- The resulting list will be sorted in ascending order by 'ix'.
-- The values in @[a]@ will be sorted in ascending order as well.
groupAscBy :: forall ix ixs a. (Coercible ix (IndexType a ix) ,OrderedContainer (IndexType a ix), IsIndexOf a ix ixs) =>  IxSet ixs a -> [(ix, [a])]
groupAscBy (IxSet _ indexes) = f (access indexes)
  where
    f :: Ix ix a -> [(ix, [a])]
    f (Ix index) = map (coerce . second Set.toAscList) (Container.toAscList index)

-- | Returns lists of elements paired with the indices determined by
-- type inference.
--
-- The resulting list will be sorted in descending order by 'ix'.
-- The values in @[a]@ will, however, be sorted in /ascending/ order.
groupDescBy :: forall ix ixs a. (OrderedContainer (IndexType a ix),Coercible ix (IndexType a ix) ,IsIndexOf a ix ixs) =>  IxSet ixs a -> [(ix, [a])]
groupDescBy (IxSet _ indexes) = f (access indexes)
  where
    f :: Ix ix a -> [(ix, [a])]
    f (Ix index) = map (coerce . second Set.toAscList) (Container.toDescList index)

-- | Returns lists of elements paired with the indices determined by
-- type inference.
--
-- The resulting list will be sorted in descending order by 'ix'.
-- The values in @[a]@ will also be sorted in descending order.
groupFullDescBy :: (OrderedContainer (IndexType a ix),Coercible ix (IndexType a ix) ,IsIndexOf a ix ixs) =>  IxSet ixs a -> [(ix, [a])]
groupFullDescBy (IxSet _ indexes) = f (access indexes)
  where
    f :: forall ix a . (OrderedContainer (IndexType a ix), Coercible ix (IndexType a ix))  =>  Ix ix a -> [(ix, [a])]
    f (Ix index) = map (coerce . second Set.toDescList) (Container.toDescList index)

-- | A function for building up selectors on 'IxSet's.  Used in the
-- various get* functions.  The set must be indexed over key type,
-- doing otherwise results in a compile error.

getEQ :: forall ixs ix a. (Eq (IndexType a ix), Indexed a ix , Indexable ixs a, IsIndexOf a ix ixs)
        => ix -> IxSet ixs a -> IxSet ixs a
getEQ v (IxSet _ ixs) =  f (access ixs)
  where
    f :: Ix ix a -> IxSet ixs a
    f (Ix index) = maybe empty (fromContainerOfSet v) $ Container.lookup (ixRep @a @ix v) index
{-# INLINE getEQ #-}

member :: forall ixs ix a. (Indexed a ix , IsIndexOf a ix ixs) 
        => ix -> IxSet ixs a -> Bool 
member v (IxSet _ ixs) =  f (access ixs)
  where
    f :: Ix ix a -> Bool 
    f (Ix index) = Container.member (ixRep @a @ix v) index
{-# INLINE member #-}


lookup :: forall ixs ix a. (Indexed a ix , IsIndexOf a ix ixs) 
        => ix -> IxSet ixs a -> Maybe a
lookup v (IxSet _ ixs) =  f (access ixs)
  where
    f :: Ix ix a -> Maybe a
    f (Ix index) = case Set.toList <$> (Container.lookup (ixRep @a @ix v) index) of
                        Just [x] -> Just x
                        _ -> Nothing
{-# INLINE lookup#-}

-- | A function for building up selectors on 'IxSet's.  Used in the
-- various get* functions.  The set must be indexed over key type,
-- doing otherwise results in a compile error.
getOrd2 :: forall ixs ix a . ( Indexed a ix 
                             , OrderedContainer (IndexType a ix)
                             , Indexable ixs a
                             , IsIndexOf a ix ixs)
        => Bool -> Bool -> Bool -> ix -> IxSet ixs a -> IxSet ixs a
getOrd2 inclt inceq incgt v (IxSet _ ixs) = f (access ixs)
  where
    f :: Ix ix a -> IxSet ixs a
    f (Ix index) = fromContainerOfSets @ixs @ix @a result
      where
        lt', gt' :: Container (IndexType a ix) (Set a)
        eq' :: Maybe (Set a)
        (lt', eq', gt') = Container.splitLookup (ixRep @a @ix v) index

        lt, gt :: Container (IndexType a ix) (Set a)
        lt = if inclt then lt' else Container.empty
        gt = if incgt then gt' else Container.empty
        eq :: Maybe (Set a)
        eq = if inceq then eq' else Nothing

        ltgt :: Container (IndexType a ix) (Set a)
        ltgt = Container.unionWith Set.union lt gt

        result :: Container (IndexType a ix) (Set a)
        result = case eq of
          Just eqset -> Container.insertWith Set.union (ixRep @a @ix v) eqset ltgt
          Nothing    -> ltgt

--------------------------------------------------------------------------
-- Joins
--------------------------------------------------------------------------

newtype Joined a b = Joined (a, b) deriving (Show, Read, Eq, Ord)

instance (Coercible (IndexType (Joined a b) ix) ix, Indexed a ix, Indexed b ix) => Indexed (Joined a b) ix where
  ixFun (Joined (a, b)) = ixFun a <> ixFun b


-- Optimization todo:
--
--   * can we avoid rebuilding the collection every time we query?
--     does laziness take care of everything?
--
--   * nicer operators?
--
--   * nice way to do updates that doesn't involve reinserting the entire data
--
--   * can we index on xpath rather than just type?

-- | Statistics about 'IxSet'. This function returns quadruple
-- consisting of
--
--   1. total number of elements in the set
--   2. number of declared indices
--   3. number of keys in all indices
--   4. number of values in all keys in all indices.
--
-- This can aid you in debugging and optimisation.
--
stats :: Indexable ixs a => IxSet ixs a -> (Int,Int,Int,Int)
stats (IxSet a ixs) = (no_elements,no_indexes,no_keys,no_values)
    where
      no_elements = Set.size a
      no_indexes  = lengthIxList ixs
      no_keys     = sum (ixListToList (\ (Ix m) -> Container.size m) ixs)
      no_values   = sum (ixListToList (\ (Ix m) -> sum [Set.size s | s <- Container.elems m]) ixs)

forceIndexesSeq :: Indexable ixs a => IxSet ixs a -> IxSet ixs a
forceIndexesSeq (IxSet a ixs)= IxSet a (mapIxList' id ixs)

forceIndexesPar :: Indexable ixs a => IxSet ixs a -> IxSet ixs a
forceIndexesPar (IxSet a ixs)= IxSet a (runEval $ mapIxListPar' ixs)
