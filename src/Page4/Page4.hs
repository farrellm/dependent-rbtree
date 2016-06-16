{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Page4.Page4
  (Tree
  ,empty
  ,insert
  ,unwindB
  ,(&)
  ) where

import Data.Type.Natural
import Page4.Types

import Debug.Trace

(&) :: a -> (a->b) -> b
x & f = f x

type Tree = Root

empty :: Root a
empty = Root Leaf

insert :: (Ord a, Show a) => Root a -> a -> Root a
insert (Root r) v = unwindR . inject v . findLeaf v $ toRootZip r

toRootZip :: Black n a -> Either (RedZip n a) (BlackZip n a)
toRootZip b = toZip (Right b) (Left RootCrumb)

toZip :: Either (Red n a) (Black n a) -> RBCrumb n a -> Either (RedZip n a) (BlackZip n a)
toZip n c =
  case n of
    (Right Leaf) -> Right (LeafZip c)
    (Right (Black2 v l r)) -> Right (Black2Zip v l r c)
    (Right (Black3 v l r)) -> Right (Black3Zip v l r c)
    (Right (Black4 v (Red lv ll lr) (Red rv rl rr))) ->
      case c of
        Right cb -> Left (RedZip v (Black2 lv ll lr) (Black2 rv rl rr) cb)
        Left cr  -> Left (TempZip v (Black2 lv ll lr) (Black2 rv rl rr) cr)
    (Left (Red v l r)) ->
      case c of
        (Right cb) -> Left (RedZip v l r cb)
        (Left cr)  -> Left (TempZip v l r cr)

findLeaf :: forall a n . (Ord a) => a -> Either (RedZip n a) (BlackZip n a) -> BlackZip Z a
findLeaf u z =
  case z of
    Right l@(LeafZip _) -> l
    Right (Black2Zip v l r c)
      | u < v     -> go (Right l) (Right $ RightBlack2Crumb v r c)
      | otherwise -> go (Right r) (Right $ LeftBlack2Crumb v l c)
    Right (Black3Zip v l r c)
      | u < v     -> go (Left l) (Right $ RightBlack2Crumb v r c)
      | otherwise -> go (Right r) (Right $ LeftBlack3Crumb v l c)
    Left (RedZip v l r c)
      | u < v ->     go (Right l) (Left $ RightRedCrumb v r c)
      | otherwise -> go (Right r) (Left $ LeftRedCrumb v l c)
    Left (TempZip v l r c)
      | u < v ->     go (Right l) (Left $ RightTempCrumb v r c)
      | otherwise -> go (Right r) (Left $ LeftTempCrumb v l c)
  where go :: Either (Red m a) (Black m a) -> RBCrumb m a -> BlackZip Z a
        go n c = findLeaf u (toZip n c)

inject :: a -> BlackZip Z a -> RedZip Z a
inject v (LeafZip (Right c)) = RedZip v Leaf Leaf c
inject v (LeafZip (Left c)) = TempZip v Leaf Leaf c

unwindB :: Show a => BlackZip n a -> Root a
unwindB z =
  case z of
    (LeafZip (Left RootCrumb)) -> Root Leaf

    (Black2Zip v l r (Left RootCrumb)) -> Root (Black2 v l r)
    (Black3Zip v l r (Left RootCrumb)) -> Root (Black3 v l r)

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

unwindR :: Show a => RedZip n a -> Root a
unwindR z = case z of
  (TempZip v l r RootCrumb) ->
    Root (Black2 v l r)

  (TempZip v l r (LeftRedCrumb a xl (RightBlack2Crumb b hr hp))) ->
    error (show z)
    -- let xr = Red v l r
    -- in case hp of
    --   Right bhp -> unwindB (Black3Zip b xr hr (Left $ LeftRedCrumb a xl bhp))
    --   Left rhp -> unwindB (Black3Zip b xr hr (Left $ LeftTempCrumb a xl rhp))
  (TempZip v l r (RightRedCrumb a xr (RightBlack2Crumb b hr hp))) ->
    let xl = Black2 v l r
    in case hp of
      Right bhp -> unwindB (Black2Zip b xr hr (Left $ LeftRedCrumb a xl bhp))
      Left rhp -> unwindB (Black2Zip b xr hr (Left $ LeftTempCrumb a xl rhp))

  (TempZip b xl xr (LeftTempCrumb a hl hc)) ->
    unwindR (TempZip a hl xl (RightTempCrumb b xr hc))
  (TempZip v l r (RightTempCrumb a xr _)) ->
    error (show z)

  (RedZip b xl xr (LeftBlack2Crumb a hl hp)) ->
    unwindB (Black3Zip b (Red a hl xl) xr hp)
  (RedZip v l r (RightBlack2Crumb pv pr pc)) ->
    unwindB (Black3Zip pv (Red v l r) pr pc)

  (RedZip v l r (LeftBlack3Crumb pv (Red plv pll plr) epc)) ->
    case epc of
      Right pc -> unwindR (RedZip pv (Black2 plv pll plr) (Black2 v l r) pc)
      Left pc -> unwindR (TempZip pv (Black2 plv pll plr) (Black2 v l r) pc)
