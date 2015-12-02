module Datatypes where


{- Data types -}

data Tile = Grass
          | Wall
          | Empty

data Map = Map Int Int [[Tile]]

type Point = (Int, Int)

data Object = Player Point
            | Chest Point

data Scene = Scene Map Object [Object]

{- Reading and Showing Tiles and Objects -}

instance Read Tile where
  readsPrec _ "." = [(Grass, "")]
  readsPrec _ "#" = [(Wall, "")]
  readsPrec _ _   = [(Empty, "")]

instance Show Tile where
  show Grass  = "."
  show Wall   = "#"
  show Empty  = " "

instance Show Object where
  show (Player _) = "☃"
  show (Chest _) = "?"

