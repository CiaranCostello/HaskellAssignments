module HaskellGame.Interaction where

import Prelude (
                Num(..), Eq(..), Show(..),
                Bool(..), Char(), Int(),
                (||), (.), otherwise, not
               )

import qualified System.Console.ANSI as Console
import qualified Data.List as List
import Data.List ((++), (!!), elem, any, filter, delete, null)

import HaskellGame.Datatypes
import HaskellGame.Graphics
import HaskellGame.Battle

{-
  Check if the player's new position would collide with something.
  Return True if there would be a collision.
  Return False if there would be no collision.
-}

detectCollision :: Scene -> Point -> Bool
detectCollision theScene (x, y) =
  let tile = ((contents (map theScene)) !! y) !! x
      objectPositions = List.map position (objects theScene)
      monsterPositions = List.map position (monsters theScene)
  in notWalkable tile || (any (== (x, y)) (objectPositions ++ monsterPositions))
  where
    notWalkable Grass = False
    notWalkable _     = True

{- Handle a key press from the player -}

handleInput :: Char -> Scene -> Scene
handleInput c theScene
  | c `elem` ['i', 'j', 'k', 'l'] = movePlayer c theScene
  | c == 'a'                      = doAttack theScene
  | otherwise                     = theScene
  where
    movePlayer :: Char -> Scene -> Scene
    movePlayer keyPressed oldScene =
      let (x, y) = position (player oldScene)
          newPosition = case keyPressed of
                          'i' -> (x, (y-1))
                          'j' -> ((x-1), y)
                          'k' -> (x, (y+1))
                          'l' -> ((x+1), y)
                          _   -> (x, y)
          newPlayer = (player oldScene) { pos = newPosition }
          isCollision = detectCollision oldScene newPosition
      in if isCollision then oldScene
         else oldScene {player = newPlayer}

missedMessage :: [Message]
missedMessage = [(Console.Red, "You flail wildly at empty space! Your attack connects with nothing.")]

hitMessage :: Monster -> Int -> Player -> Int -> [Message]
hitMessage monster monsterDamage player playerDamage =
  [(Console.Red, show monster ++ " hits " ++ show player  ++ " for " ++ show playerDamage  ++ " damage!"),
   (Console.Red, show player  ++ " hits " ++ show monster ++ " for " ++ show monsterDamage ++ " damage!")]

doAttack :: Scene -> Scene
doAttack oldScene = 
  let p = player oldScene
      monstersInRange = filter (\ m -> (distance m p) == 1) (monsters oldScene)
      newMessages = if monstersInRange == [] then 
        missedMessage
      else
        (messageMap (produceMessage p) monstersInRange)
      (newPlayer, newMonsters) = a p (monsters oldScene)
      newMessages' = (messages oldScene)++newMessages
  in oldScene {player = newPlayer, monsters = newMonsters, messages = newMessages'}

produceMessage :: Player -> Monster -> [Message]
produceMessage p m = 
  let 
      (newP, newM) = fight (p, m)
  in hitMessage m ((health m)-(health newM)) p ((health p)-(health newP))

a :: Player -> [Monster] -> (Player, [Monster])
a p [] = (p, [])
a p (m:ms) = let
    (newP, newM) = if (distance p m) == 1 then
      fight (p, m)
    else 
      (p, m)
    (newerP, newMs) = a newP ms
  in (newerP, (newM:newMs))

messageMap :: (Monster -> [Message]) -> [Monster] -> [Message]
messageMap _ [] = []
messageMap f (x:xs) = (f x)++(messageMap f xs)