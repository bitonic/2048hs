{-# LANGUAGE ScopedTypeVariables #-}
module Play2048 where

import           Prelude                               hiding (lines)
import           Control.Monad                         (forM_, when, mzero)
import           Control.Monad.ST                      (ST, runST)
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Maybe             (runMaybeT)
import qualified Data.Array                            as A
import qualified Data.Array.ST                         as A hiding (unsafeFreeze)
import qualified Data.Array.Unsafe                     as A
import           Data.List                             (nub, intercalate)
import           Data.Maybe                            (catMaybes)
import           System.Random                         (RandomGen, randomR, getStdGen)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type Score = Int

data Tile
    = Empty
    | Full Score
    deriving (Eq, Show)

isEmpty :: Tile -> Bool
isEmpty Empty         = True
isEmpty (Full _score) = False

type Row    = Int
type Column = Int

boardSize :: Int
boardSize = 4

-- TODO change to unboxed
type Board = A.Array (Row, Column) Tile

type MutableBoard s = A.STArray s (Row, Column) Tile

data MoveOrientation = Horizontal | Vertical deriving (Eq, Show)
data MoveDirection   = Normal | Reversed     deriving (Eq, Show)

type Move = (MoveOrientation, MoveDirection)

moveLeft, moveRight, moveDown, moveUp :: Move
moveLeft  = (Horizontal, Normal)
moveRight = (Horizontal, Reversed)
moveDown  = (Vertical,   Normal)
moveUp    = (Vertical,   Reversed)

ascIndices  :: [Int]
ascIndices  = [1 .. boardSize]

descIndices :: [Int]
descIndices = [boardSize, (boardSize - 1) .. 1]

emptyBoard :: Board
emptyBoard = boardFromList
    [ [Empty, Empty, Empty, Empty]
    , [Empty, Empty, Empty, Empty]
    , [Empty, Empty, Empty, Empty]
    , [Empty, Empty, Empty, Empty]
    ]

boardFromList :: [[Tile]] -> Board
boardFromList lines'
    | length lines' == boardSize && all ((== boardSize) . length) lines' =
        A.array ((1, 1), (boardSize, boardSize)) $
        concat [ [((row, col), tile) | (col, tile) <- zip ascIndices tiles]
               | (row, tiles) <- zip descIndices lines'
               ]
boardFromList _lines =
    error "boardFromList: invalid lines"

------------------------------------------------------------------------
-- Game logic
------------------------------------------------------------------------

winningScore :: Score
winningScore = 10

getBoardLines :: Board -> Move -> [[Tile]]
getBoardLines board (Horizontal, Normal) =
    [[board A.! (row, col) | col <- ascIndices]  | row <- ascIndices]
getBoardLines board (Horizontal, Reversed) =
    [[board A.! (row, col) | col <- descIndices] | row <- ascIndices]
getBoardLines board (Vertical,   Normal)   =
    [[board A.! (row, col) | row <- ascIndices]  | col <- ascIndices]
getBoardLines board (Vertical,   Reversed) =
    [[board A.! (row, col) | row <- descIndices] | col <- ascIndices]

writeBoardLines :: Move -> [[Tile]] -> Board
writeBoardLines (orientation, direction) lines = runST modify
  where
    index (outer, inner) = case orientation of
      Horizontal -> (outer, inner)
      Vertical   -> (inner, outer)

    outerIndices = ascIndices
    
    innerIndices = case direction of
        Normal   -> ascIndices
        Reversed -> descIndices

    modify :: forall s. ST s Board
    modify = do
        mutBoard :: MutableBoard s <-
            A.newArray ((1, 1), (boardSize, boardSize)) Empty
        forM_ (zip outerIndices lines) $ \(outer, line) ->
            forM_ (zip innerIndices line) $ \(inner, tile) ->
                A.writeArray mutBoard (index (outer, inner)) tile
        A.unsafeFreeze mutBoard

boardFringe :: [(Row, Column)]
boardFringe = nub $ [(row, col) | row <- [1..boardSize], col <- [1, boardSize]] ++
                    [(row, col) | col <- [1..boardSize], row <- [1, boardSize]]

newTile :: RandomGen g => g -> (Tile, g)
newTile gen0 =
    let (n :: Int, gen1) = randomR (1, 10) gen0
        score            = if n > 1 then 0 else 1
    in  (Full score, gen1)

spawnPiece :: forall g. RandomGen g => Board -> g -> Maybe (Board, g)
spawnPiece board gen0 =
    case mbTile of
        Nothing                  -> Nothing
        Just (tileIx, tile, gen) -> Just (runST (modify tileIx tile), gen)
  where
    availableTiles = catMaybes
        [ let tile = board A.! (row, col)
          in  if isEmpty tile then Just (row, col) else Nothing
        | (row, col) <- boardFringe
        ]

    mbTile =
        if null availableTiles
        then Nothing
        else let (tileIx, gen1) = randomR (0, length availableTiles - 1) gen0
                 (tile,   gen2) = newTile gen1
             in  Just (availableTiles !! tileIx, tile, gen2)

    modify :: forall s. (Row, Column) -> Tile -> ST s Board
    modify tileIx tile = do
        mutBoard :: MutableBoard s <- A.thaw board
        A.writeArray mutBoard tileIx tile
        A.unsafeFreeze mutBoard

advanceLines :: [[Tile]] -> Maybe [[Tile]]
advanceLines lines = changed $ map (collapseLine . compactLine) $ lines
  where
    changed lines' =
        if or (map (uncurry lineChanged) (zip lines lines'))
        then Just lines' else Nothing

    lineChanged []       []         = False
    lineChanged (_ : _)  []         = True
    lineChanged []       (_ : _)    = error "applyMove': the impossible happened"
    lineChanged (t : ts) (t' : ts') =
        if t == t' then lineChanged ts ts' else True

    compactLine = filter (/= Empty)

    collapseLine [] =
        []
    collapseLine (Full s : Full s' : tiles) | s == s' =
        (Full (s + 1) :) $ collapseLine tiles
    collapseLine (tile : tiles) =
        (tile :) $ collapseLine tiles

play :: RandomGen g => Board -> Move -> g -> Maybe (Board, g)
play board0 move gen0 =
     case advanceLines (getBoardLines board0 move) of
         Nothing    -> Nothing
         Just lines ->
             -- TODO Proper error handling here, we should never get
             -- a 'Nothing'.
             spawnPiece (writeBoardLines move lines) gen0

------------------------------------------------------------------------
-- Drawing
------------------------------------------------------------------------

drawBoard :: Board -> IO ()
drawBoard board = do
    forM_ descIndices $ \row -> do
        drawBorder      
        forM_ ascIndices $ \col -> do
            putStr "|"
            drawTile $ board A.! (row, col)
        putStrLn "|"
    drawBorder
  where
    drawBorder = do
        putStr "+"
        putStr $ intercalate "-" $ replicate boardSize "--"
        putStrLn "+"

    drawTile Empty = putStr "  "
    drawTile (Full score) = do
        when (score < 10) $ putStr " "
        putStr $ show score

------------------------------------------------------------------------
-- Main routine
------------------------------------------------------------------------

main :: IO ()
main = do
    gen0 <- getStdGen
    let Just (initBoard, gen1') = spawnPiece emptyBoard gen0
    go initBoard gen1'
  where
    go board0 gen0 = do
        drawBoard board0
        (board1, gen1) <- getAndApplyMove board0 gen0
        go board1 gen1

    getAndApplyMove board0 gen0 = do
        move <- getMove
        putStrLn ""
        putStrLn ""
        putStrLn ""
        case play board0 move gen0 of
            Nothing             -> getAndApplyMove board0 gen0
            Just (board1, gen1) -> return (board1, gen1)

    getMove :: IO Move
    getMove = do
        mbMove <- runMaybeT $ do
            '\ESC' <- lift $ getChar
            '['    <- lift $ getChar
            c      <- lift $ getChar
            case c of
                'D' -> return moveLeft
                'C' -> return moveRight
                'B' -> return moveDown
                'A' -> return moveUp
                _   -> mzero
        maybe getMove return mbMove