{-# LANGUAGE StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Data.IxSet.Typed as IS
-- import Data.IxSet.Typed.Container (HashPolymorphic(..))
import Control.Exception
import qualified Data.Set as Set
import Data.Maybe
import Data.Time
import qualified Data.List as L


data Account 
 = Account 
   { accId ::Int 
   , balance ::  Double
   , users ::  [String] 
   }deriving(Show)

instance Eq Account where
  i == j = accId i == accId j 

instance Ord Account where
  compare i j = compare (accId i) (accId j )

type AccountMap = IxSet '[Int,Double,String] Account

instance Indexed Account Int where
  -- type IndexType Account Int = Int
  ixFun = pure . accId 

instance Indexed Account Double where
  ixFun = pure . balance 

instance Indexed Account String where
  -- type IndexType Account String = HashPolymorphic String 
  ixFun = users 

measure t f = do
  t0 <-  getCurrentTime
  i <- evaluate f
  tf <-  getCurrentTime
  putStrLn $ t ++ ": " ++ show (diffUTCTime tf t0)

main = do 
  let i0 = empty :: AccountMap
      i1 = L.foldl' (\s i -> insert (Account (i  :: Int) 0 [replicate 36 'a'] ) s) i0 [0..200000]
      i2 = L.foldl' (\s v -> fst $ alterIx (round v  :: Int) (Just . maybe (error "no account") ((\i -> i {balance = v + 1})))  s ) i1 [0..20000]
    --  i2 = L.foldl' (\s v -> fst $ alterIx (1 :: Int) (Just . maybe (error "no account") (Set.mapMonotonic (\i -> i {balance = v + 1})))  s ) i1 [0..20000]
      i3 = L.foldl' (\s v -> updateIx (round v  :: Int) ((fromJust $ IS.lookup (round v :: Int) s)  {balance = v + 1})  s ) i1 [0..20000]

  measure "create" i1
  measure "alter" i2  
  measure "update" i3
  print (size i1)
  print (i2 == i3)

