module Board
    ( module Board.Int

    , Row
    , Column
    , size
    , empty
    , fringe
    , emptyTiles
    , ascIndices
    , descIndices
    ) where

import           Prelude hiding (read)
import           Data.List (nub)

import qualified Tile
import           Board.Common
import           Board.Int
-- Just to make sure they compile
import           Board.Vector ()
import           Board.ByteArray ()

empty :: Board
empty = fromList
    [ [Tile.empty, Tile.empty, Tile.empty, Tile.empty]
    , [Tile.empty, Tile.empty, Tile.empty, Tile.empty]
    , [Tile.empty, Tile.empty, Tile.empty, Tile.empty]
    , [Tile.empty, Tile.empty, Tile.empty, Tile.empty]
    ]

fringe :: [(Row, Column)]
fringe = nub $
    [(row, col) | row <- [1..size], col <- [1, size]] ++
    [(row, col) | col <- [1..size], row <- [1, size]]

emptyTiles :: Board -> [(Row, Column)]
emptyTiles board =
    [ (row, col)
    | (row, col) <- fringe, Tile.isEmpty (read board (row, col))
    ]
