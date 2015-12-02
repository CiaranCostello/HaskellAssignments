module HaskellGame.Battle where

import Prelude (
                Num(..), Eq(..), Ord(..),
                Int(), Maybe(..),
                (.), fst, otherwise, floor, log, sum, snd, div, fromIntegral
               )
import qualified Data.List as List
import Data.List (find)

import HaskellGame.Datatypes


sumStats :: [Stat] -> Int 
sumStats [] = 0
sumStats ((_, x):xs) = x + sumStats xs

{-
   Let's define what we need to be able to do with a Combatant.
   We need to be able to get its health, compute attack and defense,
   and deal damage to it. We'll define this interface with a type class.
-}

class Combatant a where
  health  :: a -> Int
  attack  :: a -> Int
  defense :: a -> Int
  level   :: a -> Int
  damage  :: a -> Int -> a

{-
   Now we need a battle system!
   Let's build the simplest thing that could possibly work.
   At each step of the battle, the two combatants damage each other.
-}

fight :: (Combatant a, Combatant b) => (a, b) -> (a, b)
fight (fighter1, fighter2) =
  let damageTo1 = ((attack fighter2) - (defense fighter1))
      damageTo2 = ((attack fighter1) - (defense fighter2))
  in (damage fighter1 damageTo1, damage fighter2 damageTo2)

{-
   Now we need to give instances to say how Players and Monsters implement
   the operations of the Combatant type class, so that our fight function
   can operate on them.
-}

instance Combatant Player where -- Task 2 : need to implement the five functions below
  health p = hitpoints p

  attack p = 
    case find (\(name, _) -> name == "Strength") (stats p) of
      Nothing -> 0
      Just ( _ , v ) -> case find (\(name, _) -> name == "Fisticuffs") (skills p) of
        Nothing -> 0
        Just ( _ , w) -> v * w

  defense p = 
    case find (\(name, _) -> name == "Toughness") (stats p) of
      Nothing -> 0
      Just( _ , v) -> v

  level p = floor (log (fromIntegral (sumStats (stats p))))

  damage p dmg 
    | dmg < 0 = p
    | otherwise = 
      case p of
        (Player h e s sk pos) -> (Player (h - dmg) e s sk pos)

instance Combatant Monster where
  health m =
    case m of
      (Dragon h _ _) -> h
      (Zombie h _ _) -> h

  attack m =
    case m of
      (Dragon _ a _) -> a
      (Zombie _ a _) -> a

  defense m =
    case m of
      (Dragon _ d _) -> d
      (Zombie _ d _) -> d

  level m = 1 + ((health m + attack m + defense m) `div` 3)

  damage m dmg
    | dmg < 0 = m
    | otherwise =
        case m of
          (Dragon h x y) -> (Dragon (h-dmg) x y)
          (Zombie h x y) -> (Zombie (h-dmg) x y)
