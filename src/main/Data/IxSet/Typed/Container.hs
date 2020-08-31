{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Data.IxSet.Typed.Container where

import Control.DeepSeq 

import qualified Data.Map as Map 
import qualified Data.HashMap.Lazy as HashMap 
import Data.Hashable
import qualified Data.IntMap as IntMap 

class NFData1 (Container k) =>  IxContainerMinimal k where
  type Container k = (c :: * -> * ) |   c -> k
  alter :: (Maybe a -> Maybe a) -> k -> Container k a -> Container k a
  empty :: Container k a 

class IxContainerMinimal k  =>  IxContainer k where
  singleton ::  k -> a -> Container k a
  size :: Container k a  -> Int
  elems :: Container k a -> [a]
  keys :: Container k a -> [k]
  toList :: Container k a -> [(k,a)]
  lookup :: k -> Container k a -> Maybe a 
  member :: k -> Container k a -> Bool
  insert :: k -> a -> Container k a -> Container k a
  insertWith  :: (a -> a -> a) -> k -> a -> Container k a -> Container k a
  mapMaybe :: (a -> Maybe b) -> Container k a -> Container k b
  filter :: (a -> Bool) -> Container k a  -> Container k a
  differenceWith ::  (a -> a -> Maybe a ) -> Container k a -> Container k a  -> Container k a
  unionWith :: (a -> a -> a) -> Container k a -> Container k a  -> Container k a
  intersectionWith :: (a -> a -> a) -> Container k a -> Container k a -> Container k a 
  update :: (a -> Maybe a) -> k -> Container k a -> Container k a

class IxContainer k =>  OrderedContainer k where
  toAscList :: Container k a -> [(k,a)]
  toDescList :: Container k a -> [(k,a)]
  splitLookup :: k -> Container k a -> (Container k a, Maybe a, Container k a)

newtype Polymorphic a = Polymorphic a deriving(Eq,Ord,NFData,Num,Show,Read)
newtype HashPolymorphic a = HashPolymorphic a deriving(Eq,Ord,NFData,Num,Show,Read,Hashable)

instance NFData1 (Map.Map a) where
  liftRnf _ i = seq i ()

instance  Ord a => IxContainerMinimal (Polymorphic  a) where
  type Container (Polymorphic a) = Map.Map (Polymorphic  a)
  alter = Map.alter
  {-# INLINE alter #-}
  empty = Map.empty

instance  Ord a => IxContainer (Polymorphic  a) where
  singleton = Map.singleton
  size = Map.size
  elems = Map.elems
  keys = Map.keys
  toList = Map.toList
  member = Map.member
  {-# INLINE member #-}
  lookup = Map.lookup
  {-# INLINE lookup #-}
  insert = Map.insert
  {-# INLINE insert #-}
  insertWith = Map.insertWith
  {-# INLINE insertWith #-}
  mapMaybe = Map.mapMaybe
  filter = Map.filter
  differenceWith = Map.differenceWith
  unionWith = Map.unionWith
  intersectionWith = Map.intersectionWith
  update = Map.update

instance Ord a => OrderedContainer (Polymorphic a ) where
  toAscList = Map.toAscList
  toDescList = Map.toDescList
  splitLookup = Map.splitLookup

instance NFData1 IntMap.IntMap  where
  liftRnf _ v = seq v ()

instance  IxContainerMinimal Int where
  type Container Int = IntMap.IntMap
  empty = IntMap.empty
  alter = IntMap.alter
  {-# INLINE alter #-}

instance  IxContainer Int where
  singleton = IntMap.singleton
  size = IntMap.size
  elems = IntMap.elems
  keys = IntMap.keys
  toList = IntMap.toList
  member = IntMap.member
  {-# INLINE member #-}
  lookup = IntMap.lookup
  {-# INLINE lookup #-}
  insert = IntMap.insert
  {-# INLINE insert #-}
  insertWith = IntMap.insertWith
  {-# INLINE insertWith #-}
  mapMaybe = IntMap.mapMaybe
  filter = IntMap.filter
  differenceWith = IntMap.differenceWith
  unionWith = IntMap.unionWith
  intersectionWith = IntMap.intersectionWith
  update = IntMap.update


instance  OrderedContainer Int where
  toAscList = IntMap.toAscList
  toDescList = IntMap.toDescList
  splitLookup = IntMap.splitLookup

instance NFData1 (HashMap.HashMap a) where
  liftRnf _ v = seq v ()

instance  (Eq a, Hashable a) => IxContainerMinimal (HashPolymorphic a) where
  type Container (HashPolymorphic a) = HashMap.HashMap (HashPolymorphic a)
  empty = HashMap.empty
  alter = HashMap.alter
  {-# INLINE alter #-}

instance  (Eq a, Hashable a) => IxContainer (HashPolymorphic a) where
  singleton = HashMap.singleton
  size = HashMap.size
  elems = HashMap.elems
  keys = HashMap.keys
  toList = HashMap.toList
  member = HashMap.member
  {-# INLINE member #-}
  lookup = HashMap.lookup
  {-# INLINE lookup #-}
  insert = HashMap.insert
  {-# INLINE insert #-}
  insertWith = HashMap.insertWith
  {-# INLINE insertWith #-}
  mapMaybe = HashMap.mapMaybe
  filter = HashMap.filter
  differenceWith = HashMap.differenceWith
  unionWith = HashMap.unionWith
  intersectionWith = HashMap.intersectionWith
  update = HashMap.update

