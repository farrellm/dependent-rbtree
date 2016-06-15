{-# LANGUAGE DataKinds, GADTs #-}

module Page4.Page4
  (Tree
  ,empty
  ,insert
  ,findLeaf
  ,unwindB
  ,(&)
  ) where

import Data.Type.Natural
import Page4.Types

(&) :: a -> (a->b) -> b
x & f = f x

type Tree = Root

empty :: Root a
empty = Root Leaf

insert :: (Ord a, Show a) => Root a -> a -> Root a
insert (Root r) v = unwindR . inject v . findLeafB v $ toRootZip r

toRootZip :: Black n a -> BlackZip n a
toRootZip b = toBlackZip b (Left RootCrumb)

toBlackZip :: Black n a -> RBCrumb n a -> BlackZip n a
toBlackZip Leaf c = LeafZip c
toBlackZip (Black2 v l r) c = Black2Zip v l r c
toBlackZip (Black3 v l r) c = Black3Zip v l r c
toBlackZip (Black4 v l r) c = Black4Zip v l r c

toRedZip :: Red n a -> BlackCrumb (S n) a -> RedZip n a
toRedZip (Red v l r) = RedZip v l r

findLeaf :: (Ord a) => a -> Root a -> BlackZip Z a
findLeaf v (Root b) = findLeafB v (toRootZip b)

findLeafB :: (Ord a) => a -> BlackZip n a -> BlackZip Z a
findLeafB u z =
  case z of
    (LeafZip _) -> z
    (Black2Zip v l r c)
      | u < v     -> findLeafB u $ toBlackZip l (Right $ RightBlack2Crumb v r c)
      | otherwise -> findLeafB u $ toBlackZip r (Right $ LeftBlack2Crumb v l c)
    (Black3Zip v l r c)
      | u < v     -> findLeafR u $ toRedZip l (RightBlack2Crumb v r c)
      | otherwise -> findLeafB u $ toBlackZip r (Right $ LeftBlack3Crumb v l c)
    (Black4Zip v l r (Right p)) -> findLeafR u $ RedZip v (flop l) (flop r) p
    (Black4Zip v l r (Left p)) -> findLeafR u $ TempZip v (flop l) (flop r) p
  where flop (Red v l r) = Black2 v l r

findLeafR :: (Ord a) => a -> RedZip n a -> BlackZip Z a
findLeafR u z =
  case z of
    (RedZip v l r c)
      | u < v ->     findLeafB u $ toBlackZip l (Left $ RightRedCrumb v r c)
      | otherwise -> findLeafB u $ toBlackZip r (Left $ LeftRedCrumb v l c)
    (TempZip v l r c)
      | u < v ->     findLeafB u $ toBlackZip l (Left $ RightTempCrumb v r c)
      | otherwise -> findLeafB u $ toBlackZip r (Left $ LeftTempCrumb v l c)

inject :: a -> BlackZip Z a -> RedZip Z a
inject v (LeafZip (Right c)) = RedZip v Leaf Leaf c
inject v (LeafZip (Left c)) = TempZip v Leaf Leaf c

unwindB :: Show a => BlackZip n a -> Root a
unwindB z =
  case z of
    (LeafZip (Left RootCrumb)) -> Root Leaf

    (Black2Zip v l r (Left RootCrumb)) -> Root (Black2 v l r)
    (Black3Zip v l r (Left RootCrumb)) -> Root (Black3 v l r)
    (Black4Zip v l r (Left RootCrumb)) -> Root (Black4 v l r)

    -- Leaf
    (LeafZip (Left (LeftRedCrumb pv pl pc))) ->
      unwindR (RedZip pv pl Leaf pc)
    (LeafZip (Left (RightRedCrumb pv pr pc))) ->
      unwindR (RedZip pv Leaf pr pc)

    (LeafZip (Left (LeftTempCrumb pv pl pc))) ->
      unwindR (TempZip pv pl Leaf pc)
    (LeafZip (Left (RightTempCrumb pv pr pc))) ->
      unwindR (TempZip pv Leaf pr pc)

    (LeafZip (Right (LeftBlack2Crumb pv pl pc))) ->
      unwindB (Black2Zip pv pl Leaf pc)
    (LeafZip (Right (RightBlack2Crumb pv pr pc))) ->
      unwindB (Black2Zip pv Leaf pr pc)

    (LeafZip (Right (LeftBlack3Crumb pv pl pc))) ->
      unwindB (Black3Zip pv pl Leaf pc)

    -- Black2
    (Black2Zip v l r (Left (LeftRedCrumb pv pl pc))) ->
      unwindR (RedZip pv pl (Black2 v l r) pc)
    (Black2Zip v l r (Left (RightRedCrumb pv pr pc))) ->
      unwindR (RedZip pv (Black2 v l r) pr pc)

    (Black2Zip v l r (Left (LeftTempCrumb pv pl pc))) ->
      unwindR (TempZip pv pl (Black2 v l r) pc)
    (Black2Zip v l r (Left (RightTempCrumb pv pr pc))) ->
      unwindR (TempZip pv (Black2 v l r) pr pc)

    (Black2Zip v l r (Right (LeftBlack2Crumb pv pl pc))) ->
      unwindB (Black2Zip pv pl (Black2 v l r) pc)
    (Black2Zip v l r (Right (RightBlack2Crumb pv pr pc))) ->
      unwindB (Black2Zip pv (Black2 v l r) pr pc)

    (Black2Zip v l r (Right (LeftBlack3Crumb pv pl pc))) ->
      unwindB (Black3Zip pv pl (Black2 v l r) pc)

    -- Black3
    (Black3Zip v l r (Left (LeftRedCrumb pv pl pc))) ->
      unwindR (RedZip pv pl (Black3 v l r) pc)
    (Black3Zip v l r (Left (RightRedCrumb pv pr pc))) ->
      unwindR (RedZip pv (Black3 v l r) pr pc)

    (Black3Zip v l r (Left (LeftTempCrumb pv pl pc))) ->
      unwindR (TempZip pv pl (Black3 v l r) pc)
    (Black3Zip v l r (Left (RightTempCrumb pv pr pc))) ->
      unwindR (TempZip pv (Black3 v l r) pr pc)

    (Black3Zip v l r (Right (LeftBlack2Crumb pv pl pc))) ->
      unwindB (Black2Zip pv pl (Black3 v l r) pc)
    (Black3Zip v l r (Right (RightBlack2Crumb pv pr pc))) ->
      unwindB (Black2Zip pv (Black3 v l r) pr pc)

    (Black3Zip v l r (Right (LeftBlack3Crumb pv pl pc))) ->
      unwindB (Black3Zip pv pl (Black3 v l r) pc)

    -- Black4
    (Black4Zip v l r (Left (LeftRedCrumb pv pl pc))) ->
      unwindR (RedZip pv pl (Black4 v l r) pc)
    (Black4Zip v l r (Left (RightRedCrumb pv pr pc))) ->
      unwindR (RedZip pv (Black4 v l r) pr pc)

    (Black4Zip v l r (Left (LeftTempCrumb pv pl pc))) ->
      unwindR (TempZip pv pl (Black4 v l r) pc)
    (Black4Zip v l r (Left (RightTempCrumb pv pr pc))) ->
      unwindR (TempZip pv (Black4 v l r) pr pc)

    (Black4Zip v l r (Right (LeftBlack2Crumb pv pl pc))) ->
      unwindB (Black2Zip pv pl (Black4 v l r) pc)
    (Black4Zip v l r (Right (RightBlack2Crumb pv pr pc))) ->
      unwindB (Black2Zip pv (Black4 v l r) pr pc)

    (Black4Zip v l r (Right (LeftBlack3Crumb pv pl pc))) ->
      unwindB (Black3Zip pv pl (Black4 v l r) pc)

unwindR :: Show a => RedZip n a -> Root a
unwindR z = case z of
  (TempZip v l r RootCrumb) ->
    Root (Black2 v l r)

  (TempZip _v _l _r (LeftRedCrumb _pv _pl _pc)) ->
    error (show z)
  (TempZip _v _l _r (RightRedCrumb _pv _pl _pc)) ->
    error (show z)

  (TempZip _v _l _r (LeftTempCrumb _pv _pl _pc)) ->
    error (show z)
  (TempZip _v _l _r (RightTempCrumb _pv _pl _pc)) ->
    error (show z)

  (RedZip b xl xr (LeftBlack2Crumb a hl hp)) ->
    unwindB (Black3Zip b (Red a hl xl) xr hp)
  (RedZip v l r (RightBlack2Crumb pv pr pc)) ->
    unwindB (Black3Zip pv (Red v l r) pr pc)

  (RedZip v l r (LeftBlack3Crumb pv pl pc)) ->
    unwindB (Black4Zip pv (Red v l r) pl pc)
