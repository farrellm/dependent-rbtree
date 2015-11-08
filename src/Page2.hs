{-# LANGUAGE DataKinds, GADTs, StandaloneDeriving #-}

module Page2 (Black(Leaf)) where

import Data.Type.Natural

data Red a n where
  Red :: a -> (Black a n) -> (Black a n) -> Red a n

data Black a n where
  Leaf :: Black a Z
  Black2 :: a -> (Black a n) -> (Black a n) -> Black a (S n)
  Black3 :: a -> (Red a n) -> (Black a n) -> Black a (S n)
  Black4 :: a -> (Red a n) -> (Red a n) -> Black a (S n)

deriving instance Show a => Show (Black a n)
deriving instance Show a => Show (Red a n)

data Root a where
  Root :: Black a n -> Root a
