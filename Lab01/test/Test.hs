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
chunksOf n l = (take n l):(chunksOf n (drop n l))

createMap :: Int -> Int -> String -> Map
createMap w h c =
  Map w h (chunksOf w $ map (read . (:[])) c)


{- HUnit Tests -}


test_movement_up =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      theScene = (Scene (createMap 3 4 theMap) (Player (1,2)) [])
      expectedScene = (Scene (createMap 3 4 theMap) (Player (1,1)) [])
  in
    expectedScene @=? (handleInput 'i' theScene)

test_movement_down =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      theScene = (Scene (createMap 3 4 theMap) (Player (1,1)) [])
      expectedScene = (Scene (createMap 3 4 theMap) (Player (1,2)) [])
  in
    expectedScene @=? (handleInput 'k' theScene)

test_movement_left =
  let theMap = "####" ++
               "#..#" ++
               "####"
      theScene = (Scene (createMap 4 3 theMap) (Player (2,1)) [])
      expectedScene = (Scene (createMap 3 4 theMap) (Player (1,1)) [])
  in
    expectedScene @=? (handleInput 'j' theScene)

test_movement_right =
  let theMap = "####" ++
               "#..#" ++
               "####"
      theScene = (Scene (createMap 4 3 theMap) (Player (1,1)) [])
      expectedScene = (Scene (createMap 3 4 theMap) (Player (2,1)) [])
  in
    expectedScene @=? (handleInput 'l' theScene)
    

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
  
prop_distance :: Int -> Int -> Int -> Int -> Bool
prop_distance x1 y1 x2 y2
 = distance (x1,y1) (x2,y2) == dist1 (x1,y1) (x2,y2)
   ||
   distance (x1,y1) (x2,y2) == dist2 (x1,y1) (x2,y2)
 where
   dist1 (x1,y1) (x2,y2) = abs(x1-x2)+abs(y1-y2)
   dist2 (x1,y1) (x2,y2)
     = round $ sqrt $ fromIntegral ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))

main = do { putStrLn "Test.main..." ; defaultMain tests }

tests :: [TF.Test]
tests = [
          testGroup "Movement [1 mark]" [
            testCase "Move Up"    test_movement_up
          , testCase "Move Down"  test_movement_down
          , testCase "Move Left"  test_movement_left
          , testCase "Move Right" test_movement_right
          ]
        , testGroup "takesome and dropsome [4 marks]" [
            testProperty "takesome works like Prelude.take [2 marks]" prop_takesome_take
          , testProperty "dropsome works like Prelude.drop [2 marks]" prop_dropsome_drop
          ]
        , testGroup "distance [5 marks]" [
            testProperty "distance is Manhattan or Euclidean" prop_distance
          ]
        ]
