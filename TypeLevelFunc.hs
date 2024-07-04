{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.Kind (Type)
import PeanoNat (PeanoNat (..))
import PhantomType (SizedList)

type Plus2 (n :: PeanoNat) = Succ (Succ n)

type Pred :: PeanoNat -> PeanoNat
type family Pred (n :: PeanoNat) :: PeanoNat

type instance Pred Zero = Zero

type instance Pred (Succ n) = n

pred :: PeanoNat -> PeanoNat
pred Zero = Zero
pred (Succ m) = m

type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat

type instance Add Zero m = m

type instance Add (Succ n) m = Succ (Add n m)

add :: PeanoNat -> PeanoNat -> PeanoNat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- append :: SizedList n a -> SizedList m a -> SizedList (Add n m) a

type family IsInt (t :: Type) :: Bool where
  IsInt Int = 'True
  IsInt a = 'False

type family Equal a b where
  Equal a a = 'True
  Equal a b = 'False

type family Arity (t :: Type) :: PeanoNat where
  Arity (a -> b) = Succ (Arity b)
  Arity a = Zero

type family (+) (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat

type instance (+) Zero m = m

type instance (+) (Succ n) m = Succ (n + m)

-- type family (n :: PeanoNat) + (m :: PeanoNat) :: PeanoNat

-- type instance Zero + m = m

-- type instance Succ n + m = Succ (n + m)

type Foo a b = Either
