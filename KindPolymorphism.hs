{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

import Data.Kind

-- data MyProxy1 a = MyProxy1
data MyProxy2 a = MyProxy2

data MyProxy3 (a :: k) = MyProxy3

type MyProxy4 :: k -> Type
data MyProxy4 a = MyProxy4

type MyProxy5 :: forall {k}. k -> Type
data MyProxy5 a = MyProxy5

type T :: forall {k1} k2. k1 -> k2 -> Type
data T a b = T
