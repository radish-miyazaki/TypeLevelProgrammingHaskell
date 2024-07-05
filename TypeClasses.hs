{-# LANGUAGE DataKinds #-}

import Data.Proxy
import PeanoNat (PeanoNat (..))
import PhantomType (SizedList)

class PeanoNatToInteger (n :: PeanoNat) where
  peanoNatToInteger :: Proxy n -> Integer

instance PeanoNatToInteger Zero where
  peanoNatToInteger _ = 0

instance (PeanoNatToInteger n) => PeanoNatToInteger (Succ n) where
  peanoNatToInteger _ = 1 + peanoNatToInteger (Proxy :: Proxy n)

lengthOfSizedList :: forall n a. (PeanoNatToInteger n) => SizedList n a -> Integer
lengthOfSizedList _ = peanoNatToInteger (Proxy :: Proxy n)

class CompareNat (n :: PeanoNat) (m :: PeanoNat) where
  compareNat :: Proxy n -> Proxy m -> Ordering

instance CompareNat Zero Zero where
  compareNat _ _ = EQ

instance CompareNat Zero (Succ m) where
  compareNat _ _ = LT

instance CompareNat (Succ m) Zero where
  compareNat _ _ = GT

instance (CompareNat n m) => CompareNat (Succ n) (Succ m) where
  compareNat _ _ = compareNat (Proxy :: Proxy n) (Proxy :: Proxy m)