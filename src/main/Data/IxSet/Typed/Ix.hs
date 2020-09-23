{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.IxSet.Typed.Ix
    ( Ix(..)
    , Indexed(..)
    , insert
    , lookup
    , delete
    , fromList
    , fromSet
    , insertList
    , deleteList
    , union
    , intersection
    , difference
    , filterFrom
    )
    where

import qualified Data.IxSet.Typed.Container as Container
import Data.IxSet.Typed.Container (Container,IxContainerMinimal, IxContainer,Polymorphic,HashPolymorphic) 
import Data.Coerce
import Data.Hashable
import qualified Data.List as List
import Data.IntMap  (IntMap)
import Data.Map  (Map)
import Data.HashMap.Lazy  (HashMap)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (filter, lookup)

-- the core data types
class Coercible ix (IndexType a ix) =>  Indexed a ix where
  type IndexType a ix 
  type IndexType a ix = Container.Polymorphic ix  
  ixFun :: a -> [ix]
  ixRep :: ix -> IndexType a ix
  ixRep =  coerce 
  ixFunRep::  a -> [IndexType a ix]
  ixFunRep f = ixRep @a @ix <$> ixFun f



-- | 'Ix' is a 'Container' from some key (of type 'ix') to a 'Set' of
-- values (of type 'a') for that key.
newtype Ix ix a = Ix (Container (IndexType a ix) (Set a))

-- modification operations

lookup :: IxContainerMinimal k
       => k -> Container k (Set a) -> Maybe (Set a)
lookup k v = Container.lookup k v
{-# INLINE lookup #-}
{-# SPECIALIZE lookup :: Int -> IntMap (Set a) -> Maybe (Set a) #-}
{-# SPECIALIZE lookup :: (Ord k) => Polymorphic k -> Map (Polymorphic k) (Set a) -> Maybe (Set a)  #-}
{-# SPECIALIZE lookup :: (Eq k, Hashable k) => HashPolymorphic k -> HashMap (HashPolymorphic k) (Set a) -> Maybe (Set a)  #-}


-- | Convenience function for inserting into 'Container's of 'Set's as in
-- the case of an 'Ix'.  If the key did not already exist in the
-- 'Container', then a new 'Set' is added transparently.
insert :: (IxContainerMinimal k ,Ord a)
       => k -> a -> Container k (Set a) -> Container k (Set a)
insert k v index = Container.alter (Just . maybe (Set.singleton v) (Set.insert v)) k index
{-# INLINE insert #-}
{-# SPECIALIZE insert :: Ord a =>  Int -> a -> IntMap (Set a) -> IntMap (Set a) #-}
{-# SPECIALIZE insert :: (Ord a, Ord k) => Polymorphic k -> a -> Map (Polymorphic k) (Set a) -> Map (Polymorphic k) (Set a)  #-}
{-# SPECIALIZE insert :: (Ord a, Eq k, Hashable k) => HashPolymorphic k -> a -> HashMap (HashPolymorphic k) (Set a) -> HashMap (HashPolymorphic k) (Set a)  #-}

-- | Helper function to 'insert' a list of elements into a set.
insertList :: (IxContainerMinimal k ,Ord a)
           => [(k,a)] -> Container k (Set a) -> Container k (Set a)
insertList xs index = List.foldl' (\m (k,v)-> insert k v m) index xs
{-# INLINE insertList #-}
{-# SPECIALIZE insertList :: Ord a =>  [(Int ,a)] -> IntMap (Set a) -> IntMap (Set a) #-}
{-# SPECIALIZE insertList :: (Ord a, Ord k) => [(Polymorphic k,a)] -> Map (Polymorphic k) (Set a) -> Map (Polymorphic k) (Set a)  #-}
{-# SPECIALIZE insertList :: (Ord a, Eq k, Hashable k) => [(HashPolymorphic k,a)] -> HashMap (HashPolymorphic k) (Set a) -> HashMap (HashPolymorphic k) (Set a)  #-}

-- | Helper function to create a new index from a list.
fromList :: (IxContainerMinimal k ,Ord a) => [(k, a)] -> Container k (Set a)
fromList xs =
  List.foldl' (\m (k,v) -> insert k v m)  Container.empty  xs
{-# INLINE fromList #-}
{-# SPECIALIZE fromList :: Ord a =>  [(Int ,a)] -> IntMap (Set a) #-}
{-# SPECIALIZE fromList :: (Ord a, Ord k) => [(Polymorphic k,a)] -> Map (Polymorphic k) (Set a)  #-}
{-# SPECIALIZE fromList :: (Ord a, Eq k, Hashable k) => [(HashPolymorphic k,a)] -> HashMap (HashPolymorphic k) (Set a)  #-}

fromSet :: forall a k. (IxContainerMinimal k,  Ord a) =>  (a -> [k] )-> Set a ->Container k (Set a)
fromSet f xs = fromList (concatMap (\i ->(,i) <$> f i ) (Set.toList xs ))
{-# INLINE fromSet #-}
{-# SPECIALIZE fromSet ::  Ord a =>  (a -> [Int] ) -> Set a -> IntMap (Set a) #-}
{-# SPECIALIZE fromSet :: (Ord a, Ord k) => (a -> [Polymorphic k] ) ->Set a ->  Map (Polymorphic k) (Set a)  #-}
{-# SPECIALIZE fromSet :: (Ord a, Eq k, Hashable k) => (a -> [HashPolymorphic k] ) ->Set a -> HashMap (HashPolymorphic k) (Set a) #-}



-- | Convenience function for deleting from 'Container's of 'Set's. If the
-- resulting 'Set' is empty, then the entry is removed from the 'Container'.
delete :: (IxContainerMinimal k ,Ord a)
       => k -> a -> Container k (Set a) -> Container k (Set a)
delete k v index = Container.alter remove k index
    where
    remove Nothing  = Nothing
    remove (Just set) = let set' = Set.delete v set
                 in if Set.null set' then Nothing else Just set'
{-# INLINE delete #-}
{-# SPECIALIZE delete :: Ord a =>  Int -> a -> IntMap (Set a) -> IntMap (Set a) #-}
{-# SPECIALIZE delete :: (Ord a, Ord k) => Polymorphic k -> a -> Map (Polymorphic k) (Set a) -> Map (Polymorphic k) (Set a)  #-}
{-# SPECIALIZE delete :: (Ord a, Eq k, Hashable k) => HashPolymorphic k -> a -> HashMap (HashPolymorphic k) (Set a) -> HashMap (HashPolymorphic k) (Set a)  #-}

-- | Helper function to 'delete' a list of elements from a set.
deleteList :: (IxContainerMinimal k , Ord a)
           => [(k,a)] -> Container k (Set a) -> Container k (Set a)
deleteList xs index = List.foldl' (\m (k,v) -> delete k v m) index xs
{-# INLINE deleteList #-}

-- | Takes the union of two sets.
union :: (IxContainer k , Ord a)
       => Container k (Set a) -> Container k (Set a) -> Container k (Set a)
union index1 index2 = Container.unionWith Set.union index1 index2
{-# INLINE union #-}

-- | Takes the intersection of two sets.
intersection :: (IxContainer k , Ord a)
             => Container k (Set a) -> Container k (Set a) -> Container k (Set a)
intersection index1 index2 = Container.filter (not . Set.null) $
                             Container.intersectionWith Set.intersection index1 index2
{-# INLINE intersection #-}

-- | Takes the difference of two sets.
difference :: (IxContainer k ,Ord a)
             => Container k (Set a) -> Container k (Set a) -> Container k (Set a)
difference index1 index2 = Container.differenceWith diff index1 index2
  where diff set1 set2 = let diffSet = Set.difference set1 set2 in
                            if Set.null diffSet then Nothing else Just diffSet
{-# INLINE difference #-}

-- | Filters the sets by restricting to the elements in the provided set.
filterFrom :: (IxContainer k ,Ord a) => Set a -> Container k (Set a) -> Container k (Set a)
filterFrom s index = Container.mapMaybe g index
  where g set = let set' = Set.intersection set s
                in if Set.null set' then Nothing else Just set'
{-# INLINE filterFrom #-}
