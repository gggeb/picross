{-# LANGUAGE TemplateHaskell #-}

module Logic
    ( Block (..)
    , Direction (..)
    , Grid
    , Point
    , Cursor
    , GameState (..)
    , Game (..)
    , goal
    , grid
    , cursor
    , state
    , genEmpty
    , genLineLengths
    , moveCursor
    , updateBlock
    , toggleState
    , check
    , run
    ) where

import Utils

import Lens.Micro
import Lens.Micro.TH

data Block = Full | Marked | Empty deriving (Eq)

data Direction = UpDir | RightDir | DownDir | LeftDir deriving (Eq)

type Grid = [[Block]]

type Point  = (Int, Int)
type Cursor = Point

data GameState = Finished | Unfinished deriving (Eq)

data Game = Game
    { _goal   :: Grid
    , _grid   :: Grid
    , _cursor :: Cursor
    , _state  :: GameState
    }

makeLenses ''Game

genEmpty (x, y) = replicate y (replicate x Empty)

genLineLengths g = map (filter (/=0) . folder) g
  where
    folder xs = foldl (\acc x -> if x == Full
                                 then (init acc) ++ [last acc + 1]
                                 else acc ++ [0]) [0] xs

moveCursor d g@(Game _ gr@(h: _) (x, y) _)
    | d == UpDir    = over (cursor._2) (\v -> bound 0 maxY (v - 1)) g
    | d == RightDir = over (cursor._1) (\v -> bound 0 maxX (v + 1)) g
    | d == DownDir  = over (cursor._2) (\v -> bound 0 maxY (v + 1)) g
    | d == LeftDir  = over (cursor._1) (\v -> bound 0 maxX (v - 1)) g
  where
    maxX = length h - 1
    maxY = length gr - 1

updateBlock n g@(Game _ gr (x, y) _) = set grid (replaceAt y (replaceAt x nv (gr !! y)) gr) g
  where
    ov = (gr !! y) !! x
    nv
        | ov == n     = Empty
        | ov /= Empty = ov
        | otherwise   = n

toggleState g@(Game _ _ _ s) = set state toggled g
  where
    toggled
        | s == Finished = Unfinished
        | otherwise     = Finished

check g@(Game go gr _ _)
    | (map (map f) gr) == go  = toggleState g
    | otherwise               = g
  where
    f Full = Full
    f _    = Empty

run f g@(Game _ _ _ s)
    | s == Unfinished = f g
    | otherwise       = g
