module Tile
    ( Score
    , maxScore
    , Tile
    , empty 
    , full
    , isEmpty
    , isFull
    ) where

import           Data.Int (Int8)

------------------------------------------------------------------------
-- Tile
------------------------------------------------------------------------

type Score = Int8

maxScore :: Score
maxScore = maxBound

data Tile
    = Empty
    | Full Score
    deriving (Eq, Show)

empty :: Tile
empty = Empty

-- TODO remove the checks if this gets expensive
full :: Score -> Tile
full s | s < 0     = error "tileFull: negative argument"
       | otherwise = Full s

isEmpty :: Tile -> Bool
isEmpty Empty    = True
isEmpty (Full _) = False

isFull :: Tile -> Maybe Score
isFull Empty    = Nothing
isFull (Full s) = Just s
