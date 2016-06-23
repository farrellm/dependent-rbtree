{-# LANGUAGE DataKinds, GADTs, StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}

module Page5.Types
  (Red(..)
  ,Black(..)
  ,Tree(..)
  ,RBCrumb
  ,RedCrumb(..)
  ,BlackCrumb(..)
  ,RedZip(..)
  ,BlackZip(..)
  ) where

import Data.Type.Natural

data Red n a where
  Red :: a -> Black n a -> Black n a -> Red n a

data Black n a where
  Leaf :: Black Z a
  Black2 :: a -> Black n a -> Black n a -> Black (S n) a
  Black3 :: a -> Red n a -> Black n a -> Black (S n) a
  Black4 :: a -> Red n a -> Red n a -> Black (S n) a

deriving instance Show a => Show (Black n a)
deriving instance Functor (Red n)
deriving instance Traversable (Red n)

instance Foldable (Red n) where
  foldMap f (Red v l r) = foldMap f l `mappend` f v `mappend` foldMap f r

deriving instance Show a => Show (Red n a)
deriving instance Functor (Black n)
deriving instance Traversable (Black n)

instance Foldable (Black n) where
  foldMap _ Leaf = mempty
  foldMap f (Black2 v l r) = foldMap f l `mappend` f v `mappend` foldMap f r
  foldMap f (Black3 v l r) = foldMap f l `mappend` f v `mappend` foldMap f r
  foldMap f (Black4 v l r) = foldMap f l `mappend` f v `mappend` foldMap f r

data Tree a where
  Tree :: Black n a -> Tree a

deriving instance Show a => Show (Tree a)
deriving instance Functor Tree
deriving instance Foldable Tree
deriving instance Traversable Tree

type RBCrumb n a = Either (RedCrumb n a) (BlackCrumb (S n) a)

data RedCrumb n a where
        RootCrumb :: RedCrumb n a
        LeftRedCrumb ::
          a -> Black n a -> BlackCrumb (S n) a -> RedCrumb n a
        RightRedCrumb ::
          a -> Black n a -> BlackCrumb (S n) a -> RedCrumb n a

data BlackCrumb n a where
        LeftBlack2Crumb ::
          a -> Black n a -> RBCrumb (S n) a -> BlackCrumb (S n) a
        RightBlack2Crumb ::
          a -> Black n a -> RBCrumb (S n) a -> BlackCrumb (S n) a
        LeftBlack3Crumb ::
          a -> Red n a -> RBCrumb (S n) a -> BlackCrumb (S n) a
        -- RightBlack3Crumb is semantically equivalent ot RightBlack2Crumb

deriving instance Show a => Show (BlackCrumb n a)
deriving instance Show a => Show (RedCrumb n a)

data RedZip n a where
        RedZip ::
          a -> Black n a -> Black n a -> BlackCrumb (S n) a -> RedZip n a
        TempZip ::
          a -> Black n a -> Black n a -> RedCrumb n a -> RedZip n a

data BlackZip n a where
        LeafZip :: RBCrumb Z a -> BlackZip Z a
        Black2Zip ::
          a -> Black n a -> Black n a -> RBCrumb (S n) a -> BlackZip (S n) a
        Black3Zip ::
          a -> Red n a -> Black n a -> RBCrumb (S n) a -> BlackZip (S n) a

deriving instance Show a => Show (BlackZip n a)
deriving instance Show a => Show (RedZip n a)
