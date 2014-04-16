{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Board.ByteArray
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
import qualified Data.Primitive.ByteArray              as BA
import           Data.Functor                          ((<$>))
import qualified Data.Hashable                         as Hashable
import           Data.Int                              (Int8)

import           Tile (Tile)
import           Board.Common

byteArrayLen :: Int
byteArrayLen = size * size

newtype Board = Board (BA.ByteArray)

instance Eq Board where
    Board b1 == Board b2 = and $
        [ (BA.indexByteArray b1 ix :: Int8) == BA.indexByteArray b2 ix
        | ix <- [0 .. byteArrayLen - 1]
        ]

instance Hashable.Hashable Board where
    hashWithSalt salt (Board (BA.ByteArray ba)) =
        Hashable.hashByteArrayWithSalt ba 0 byteArrayLen salt

read :: Board -> (Row, Column) -> Tile
read (Board board) ix =
    tileFromInt8 $ BA.indexByteArray board (boardIx ix)

type Mutable s = BA.MutableByteArray s

thaw :: Board -> ST s (Mutable s)
thaw (Board board) = do
    mutBoard <- BA.newByteArray byteArrayLen
    BA.copyByteArray mutBoard 0 board 0 byteArrayLen
    return mutBoard

unsafeFreeze :: Mutable s -> ST s Board
unsafeFreeze mutBoard = Board <$> BA.unsafeFreezeByteArray mutBoard

write :: Mutable s -> (Row, Column) -> Tile -> ST s ()
write mutBoard ix = BA.writeByteArray mutBoard (boardIx ix) . tileToInt8

fromList :: [[Tile]] -> Board
fromList lines
    | length lines == size && all ((== size) . length) lines =
        runST create
  where
    create :: forall s. ST s Board
    create = do
        mutBoard :: Mutable s <- BA.newByteArray byteArrayLen
        forM_ (zip descIndices lines) $ \(row, line) ->
            forM_ (zip ascIndices line) $ \(col, tile) ->
                write mutBoard (row, col) tile
        unsafeFreeze mutBoard
fromList _lines =
    error "boardFromList: invalid lines"
