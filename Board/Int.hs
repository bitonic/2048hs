{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Board.Int
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
import           Data.Functor                          ((<$>))
import qualified Data.Hashable                         as Hashable
import           Data.Bits                             (shiftL, shiftR, (.&.), (.|.), complement)
import           Data.Word                             (Word64)
import           Data.STRef                            (STRef, modifySTRef', newSTRef, readSTRef)

import           Tile (Tile)
import           Board.Common

newtype Board = Board Word64
    deriving (Eq, Hashable.Hashable)


read :: Board -> (Row, Column) -> Tile
read (Board board) (row, col) =
    tileFromInt8 $ (\x -> x - 1) $ fromIntegral $ (board .&. mask) `shiftR` shiftBy
  where
    ix      = boardIx (row, col)
    shiftBy = 4 * ix
    mask    = 0x000000000000000f `shiftL` shiftBy

type Mutable s = STRef s Word64

thaw :: Board -> ST s (Mutable s)
thaw (Board board) = newSTRef board

unsafeFreeze :: Mutable s -> ST s Board
unsafeFreeze mutBoard = Board <$> readSTRef mutBoard

-- TODO risk of overflow
write :: Mutable s -> (Row, Column) -> Tile -> ST s ()
write mutBoard (row, col) tile = modifySTRef' mutBoard $ \board ->
    (board .&. mask) .|. tileWord
  where
    ix       = boardIx (row, col)
    shiftBy  = 4 * ix
    mask     = complement (0x000000000000000f `shiftL` shiftBy)
    tileWord = (fromIntegral $ (+ 1) $ tileToInt8 tile) `shiftL` shiftBy

fromList :: [[Tile]] -> Board
fromList lines
    | length lines == size && all ((== size) . length) lines =
        runST create
  where
    create :: forall s. ST s Board
    create = do
        mutBoard :: Mutable s <- newSTRef 0
        forM_ (zip descIndices lines) $ \(row, line) ->
            forM_ (zip ascIndices line) $ \(col, tile) ->
                write mutBoard (row, col) tile
        unsafeFreeze mutBoard
fromList _lines =
    error "boardFromList: invalid lines"
