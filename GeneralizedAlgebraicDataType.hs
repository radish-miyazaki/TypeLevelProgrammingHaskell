{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

import Control.Monad (when)
import Data.Kind
import Data.Proxy
import PeanoNat (PeanoNat (..))

data Expr (a :: Type) where
  Const :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Equal :: Expr Int -> Expr Int -> Expr Bool
  IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a

data SizedList (n :: PeanoNat) a where
  Nil :: SizedList Zero a
  Cons :: a -> SizedList n a -> SizedList (Succ n) a

eval :: Expr a -> a
eval e = case e of
  Const x ->
    {- ここでは a が Int になっている -}
    x
  Add e1 e2 ->
    {- ここでは a が Int になっている -}
    eval e1 + eval e2
  Equal e1 e2 ->
    {- ここでは a が Bool になっている -}
    eval e1 == eval e2
  IfThenElse cond then_ else_ ->
    {- ここでは a は抽象的な型 a のまま  -}
    if eval cond then eval then_ else eval else_

type (:~:) :: k -> k -> Type
data a :~: b where
  Refl :: a :~: a

instance Show (a :~: b) where
  show Refl = "Refl"

class SameNat (n :: PeanoNat) (m :: PeanoNat) where
  sameNat :: Proxy n -> Proxy m -> Maybe (n :~: m)

instance SameNat Zero Zero where
  sameNat _ _ = Just Refl

instance SameNat Zero (Succ m) where
  sameNat _ _ = Nothing

instance SameNat (Succ n) Zero where
  sameNat _ _ = Nothing

instance (SameNat n m) => SameNat (Succ n) (Succ m) where
  sameNat _ _ = case sameNat (Proxy :: Proxy n) (Proxy :: Proxy m) of
    Just Refl ->
      {- ここでは n と m が同じ型になっている-}
      Just Refl {- ここの Refl は Succ n :~: Succ m 型だが、n と m が同じ型なのでコンパイルが通る -}
    Nothing -> Nothing

data SomeSizedList a where
  SomeSizedList :: SizedList n a -> SomeSizedList a

filter' :: (a -> Bool) -> SizedList n a -> SomeSizedList a
filter' f Nil = SomeSizedList Nil
filter' f (Cons x xs)
  | f x = case filter' f xs of
      SomeSizedList ys -> SomeSizedList (Cons x ys)
  | otherwise = filter' f xs

data Showable = forall a. (Show a) => Showable a

showSomething :: Showable -> String
showSomething (Showable x) = show x

data ShowWitness a = (Show a) => ShowWitness

showSomethingWitness :: ShowWitness a -> a -> String
showSomethingWitness x y = case x of
  ShowWitness ->
    {- ここでは a は Show インスタンス -}
    show y

data Expr' a
  = (a ~ Int) => Const' Int
  | (a ~ Int) => Add' (Expr Int) (Expr Int)
  | (a ~ Bool) => Equal' (Expr Int) (Expr Int)
  | IfThenElse' (Expr Bool) (Expr a) (Expr a)

data SizedList' n a
  = (n ~ Zero) => Nil'
  | forall n'. (n ~ Succ n') => Cons' a (SizedList' n' a)