{-# LANGUAGE DataKinds, GADTs, StandaloneDeriving #-}

module Page3 (Black(Leaf)) where

import Data.Type.Natural

data Red a n where
  Red :: a -> (Black a n) -> (Black a n) -> Red a n

data Black a n where
  Leaf :: Black a Z
  Black2 :: a -> (Black a n) -> (Black a n) -> Black a (S n)
  Black3 :: a -> (Red a n) -> (Black a n) -> Black a (S n)
  Black4 :: a -> (Red a n) -> (Red a n) -> Black a (S n)

type RB a n = Either (Red a n) (Black a n)

deriving instance Show a => Show (Black a n)
deriving instance Show a => Show (Red a n)

data Root a where
  Root :: Black a n -> Root a

deriving instance Show a => Show (Root a)


type RBCrumb a n = Either (RedCrumb a n) (BlackCrumb a (S n))

data RedCrumb a n where
  RootCrumb :: RedCrumb a n
  RedCrumb :: a -> (Black a n) -> (BlackCrumb a (S n)) -> RedCrumb a n

data BlackCrumb a n where
  BlackCrumb :: a -> (RB a n) -> (RBCrumb a (S n)) -> BlackCrumb a (S n)

deriving instance Show a => Show (BlackCrumb a n)
deriving instance Show a => Show (RedCrumb a n)

type RBZip a n = Either (RedZip a n) (BlackZip a n)

data RedZip a n where
  RedZip :: a -> (Black a n) -> (Black a n) -> (BlackCrumb a (S n)) -> RedZip a n

data BlackZip a n where
  LeafZip :: (RBCrumb a n) -> BlackZip a Z
  Black2Zip :: a -> (Black a n) -> (Black a n) -> (RBCrumb a (S n)) -> BlackZip a (S n)
  Black3Zip :: a -> (Red a n) -> (Black a n) -> (RBCrumb a (S n)) -> BlackZip a (S n)
  Black4Zip :: a -> (Red a n) -> (Red a n) -> (RBCrumb a (S n)) -> BlackZip a (S n)

deriving instance Show a => Show (BlackZip a n)
deriving instance Show a => Show (RedZip a n)

toRootZip :: Black a n -> BlackZip a n
toRootZip b = toBlackZip b (Left RootCrumb)

toBlackZip :: Black a n -> RBCrumb a n -> BlackZip a n
toBlackZip Leaf c = LeafZip $ c
toBlackZip (Black2 v l r) c = Black2Zip v l r c
toBlackZip (Black3 v l r) c = Black3Zip v l r c
toBlackZip (Black4 v l r) c = Black4Zip v l r c

toRedZip :: Red a n -> BlackCrumb a (S n) -> RedZip a n
toRedZip (Red v l r) c = RedZip v l r c

findLeafB :: (Ord a) => a -> BlackZip a n -> BlackZip a Z
findLeafB u z@(LeafZip c) = z
findLeafB u (Black2Zip v l r c)
  | u < v     = findLeafB u $ toBlackZip l (Right $ BlackCrumb v (Right r) c)
  | otherwise = findLeafB u $ toBlackZip r (Right $ BlackCrumb v (Right l) c)
findLeafB u (Black3Zip v l r c)
  | u < v     = findLeafR u . toRedZip l $ BlackCrumb v (Right r) c
  | otherwise = findLeafB u $ toBlackZip r (Right $ BlackCrumb v (Left l) c)
findLeafB u (Black4Zip v l r c)
  | u < v     = findLeafR u . toRedZip l $ BlackCrumb v (Left r) c
  | otherwise = findLeafR u $ toRedZip r $ BlackCrumb v (Left r) c

findLeafR :: (Ord a) => a -> RedZip a n -> BlackZip a Z
findLeafR u (RedZip v l r c)
  | u < v     = findLeafB u $ toBlackZip l (Left $ RedCrumb v r c)
  | otherwise = findLeafB u $ toBlackZip r (Left $ RedCrumb v l c)

-- inject :: a -> BlackZip a Z -> RedZip a Z
-- inject v (LeafZip c) = RedZip v Leaf Leaf c
