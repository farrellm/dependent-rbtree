module Lib
    ( someFunc
    ) where

import Data.List
import Data.Foldable
import Test.QuickCheck

import Page2 ()
import Page3 ()
import qualified Page4.Page4 as RB

prop_sort :: Property
prop_sort = forAll (listOf $ elements ([1..1000] :: [Int])) $ \xs -> sort xs == toList (rb xs)
  where rb :: [Int] -> RB.Tree Int
        rb = foldl' RB.insert RB.empty

someFunc :: IO ()
someFunc = putStrLn "someFunc"
