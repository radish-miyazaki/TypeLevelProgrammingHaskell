{-# LANGUAGE DataKinds #-}

import Data.Proxy (Proxy)
import PeanoNat (PeanoNat (..))

newtype SizedList (n :: PeanoNat) a = SizedList [a]

safeHead :: SizedList ('Succ m) a -> a
safeHead (SizedList xs) = head xs

data Expr a
  = Const Int
  | Add (Expr Int) (Expr Int)
  | Equal (Expr Int) (Expr Int)
  | IfThenElse (Expr Bool) (Expr a) (Expr a)

mkConst :: Int -> Expr Int
mkConst = Const

mkAdd :: Expr Int -> Expr Int -> Expr Int
mkAdd = Add

mkEqual :: Expr Int -> Expr Int -> Expr Bool
mkEqual = Equal

maxBoundAsInteger :: forall a. (Integral a, Bounded a) => Proxy a -> Integer
maxBoundAsInteger _proxy = toInteger (maxBound :: a)
