module Board.Common
    ( size
    , Row
    , Column
    , ascIndices
    , descIndices
    , boardIx
    , tileToInt8
    , tileFromInt8
    ) where

import           Data.Int (Int8)

import qualified Tile as Tile

size :: Int
size = 4

type Row    = Int
type Column = Int

ascIndices  :: [Int]
ascIndices  = [1 .. size]

descIndices :: [Int]
descIndices = [size, (size - 1) .. 1]

boardIx :: (Row, Column) -> Int
boardIx (row0, col0)
    | row >= 0 && row < size && col >= 0 && col < size =
        size * row + col
    where (row, col) = (row0 - 1, col0 - 1)
boardIx ix = error $ "boardIx: invalid index " ++ show ix

tileToInt8 :: Tile.Tile -> Int8
tileToInt8 t = case Tile.isFull t of
    Nothing -> -1
    Just s  -> s

tileFromInt8 :: Int8 -> Tile.Tile
tileFromInt8 s = if s == -1 then Tile.empty else Tile.full s
