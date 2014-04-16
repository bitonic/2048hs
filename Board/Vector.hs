{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Board.Vector
    ( Board
    , read
    , fromList

    , Mutable
    , write
    , thaw
    , unsafeFreeze
    ) where

import           Prelude                               hiding (read, lines)

import           Control.Monad                         (forM_)
import           Control.Monad.ST                      (ST, runST)
import qualified Data.Vector.Unboxed                   as V
import qualified Data.Vector.Unboxed.Mutable           as V
import           Data.Functor                          ((<$>))
import qualified Data.Hashable                         as Hashable
import           Data.Int                              (Int8)

import           Tile (Tile)
import           Board.Common

vecLen :: Int
vecLen = size * size

newtype Board = Board (V.Vector Int8)
    deriving (Eq)

instance Hashable.Hashable Board where
    hashWithSalt salt (Board board) = V.foldl' Hashable.hashWithSalt salt board

read :: Board -> (Row, Column) -> Tile
read (Board board) ix = tileFromInt8 $ V.unsafeIndex board (boardIx ix)

type Mutable s = V.MVector s Int8

thaw :: Board -> ST s (Mutable s)
thaw (Board board) = V.thaw board

unsafeFreeze :: Mutable s -> ST s Board
unsafeFreeze mutBoard = Board <$> V.unsafeFreeze mutBoard

write :: Mutable s -> (Row, Column) -> Tile -> ST s ()
write mutBoard ix = V.unsafeWrite mutBoard (boardIx ix) . tileToInt8

fromList :: [[Tile]] -> Board
fromList lines
    | length lines == size && all ((== size) . length) lines =
        runST create
  where
    create :: forall s. ST s Board
    create = do
        mutBoard :: Mutable s <- V.new vecLen
        forM_ (zip descIndices lines) $ \(row, line) ->
            forM_ (zip ascIndices line) $ \(col, tile) ->
                write mutBoard (row, col) tile
        unsafeFreeze mutBoard
fromList _lines =
    error "boardFromList: invalid lines"
