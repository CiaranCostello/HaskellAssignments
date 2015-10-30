{-# LANGUAGE StandaloneDeriving #-}

module TestMain where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Main -- hiding (main)

deriving instance Show Scene
deriving instance Show Map
--deriving instance Show Object

deriving instance Eq Scene
deriving instance Eq Map
deriving instance Eq Object
deriving instance Eq Tile

{- HUnit Tests -}


test_movement =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      theScene = (Scene (createMap 3 4 theMap) (Player (1,1)) [])
      expectedScene = (Scene (createMap 3 4 theMap) (Player (1,2)) [])
  in
    expectedScene @=? (handleInput 'k' theScene)

{- QuickCheck Tests -}

prop_takesome_take :: Int -> Bool
prop_takesome_take n =
  let xs = [0..9]
  in takesome n xs == take n xs

prop_dropsome_drop :: Int -> Bool
prop_dropsome_drop n =
  let xs = [0..9]
  in dropsome n xs == drop n xs

xmain = defaultMain tests

tests :: [TF.Test]

tests = [
          testGroup "Player Movement" [
            testCase "Player Movement [0 marks]" test_movement
          ],

          testGroup "takesome and dropsome" [
            testProperty "takesome works like Prelude.take" prop_takesome_take,
            testProperty "dropsome works like Prelude.drop" prop_dropsome_drop
          ]
        ]

