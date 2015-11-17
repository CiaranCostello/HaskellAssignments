{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck  -- ((==>))
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Datatypes
import Submission
--import HaskellGame.Datatypes
--import HaskellGame.Utils
--import HaskellGame.Interaction

deriving instance Show Scene
deriving instance Show Map
-- deriving instance Show Object

deriving instance Eq Scene
deriving instance Eq Map
deriving instance Eq Object
deriving instance Eq Tile

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l = (takesome n l):(chunksOf n (dropsome n l))

createMap :: Int -> Int -> String -> Map
createMap w h c =
  Map w h (chunksOf w $ map (read . (:[])) c)


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

prop_takesome_take :: Int -> Property
prop_takesome_take n =
 n >= 0 ==>
  let xs = [0..9]
  in takesome n xs == take n xs

prop_dropsome_drop :: Int -> Property
prop_dropsome_drop n =
 n >= 0 ==>
  let xs = [0..9]
  in dropsome n xs == drop n xs

main = do { putStrLn "Test.main..." ; defaultMain tests }

tests :: [TF.Test]
tests = [
          testGroup "Test Collisions" [
            testCase "Player Movement [0 marks]" test_movement
          ],

          testGroup "takesome and dropsome" [
            testProperty "takesome works like Prelude.take" prop_takesome_take,
            testProperty "dropsome works like Prelude.drop" prop_dropsome_drop
          ]
        ]
