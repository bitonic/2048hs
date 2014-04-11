{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Prelude                               hiding (lines)

import           Control.Monad                         (forM_, when, mzero)
import           Control.Monad.ST                      (ST, runST)
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Maybe             (runMaybeT)
import qualified Data.Array.ST                         as A hiding (unsafeFreeze)
import qualified Data.Array.Unsafe                     as A
import qualified Data.Array.Unboxed                    as A
import qualified Data.Hashable                         as Hashable
import           Data.Char                             (isSpace)
import           Data.Int                              (Int8)
import           Data.List                             (nub, intercalate, sortBy, intersperse)
import           Data.List.NonEmpty                    (NonEmpty(..), toList)
import           Data.List.Split                       (splitOn, chunksOf)
import           Data.Ord                              (comparing)
import           Safe                                  (headMay)
import           System.Random                         (RandomGen, randomR, getStdGen)

------------------------------------------------------------------------
-- Tile
------------------------------------------------------------------------

type Score = Int8

newtype Tile = Tile {unTile :: Score}
    deriving (Eq, Hashable.Hashable, A.IArray A.UArray)

instance Show Tile where
    showsPrec d tile =
        case tileIsFull tile of
            Nothing -> showString "Empty"
            Just s  -> showParen (d > 11) $ showString "Full " . showsPrec 11 s

tileEmpty :: Tile
tileEmpty = Tile (-1)

-- TODO remove the checks if this gets expensive
tileFull :: Score -> Tile
tileFull s | s < 0     = error "tileFull: negative argument"
           | otherwise = Tile s

tileIsEmpty :: Tile -> Bool
tileIsEmpty (Tile s) = s < 0

tileIsFull :: Tile -> Maybe Score
tileIsFull (Tile s) = if s >= 0 then Just s else Nothing

------------------------------------------------------------------------
-- Board
------------------------------------------------------------------------

type Row    = Int
type Column = Int

boardSize :: Int
boardSize = 4

type Board = A.UArray (Row, Column) Tile

type MutableBoard s = A.STArray s (Row, Column) Tile

------------------------------------------------------------------------
-- Move
------------------------------------------------------------------------

data MoveOrientation = Horizontal | Vertical deriving (Eq, Show)
data MoveDirection   = Normal | Reversed     deriving (Eq, Show)

type Move = (MoveOrientation, MoveDirection)

moveLeft, moveRight, moveDown, moveUp :: Move
moveLeft  = (Horizontal, Normal)
moveRight = (Horizontal, Reversed)
moveDown  = (Vertical,   Normal)
moveUp    = (Vertical,   Reversed)

------------------------------------------------------------------------
-- Board handling and creation
------------------------------------------------------------------------

ascIndices  :: [Int]
ascIndices  = [1 .. boardSize]

descIndices :: [Int]
descIndices = [boardSize, (boardSize - 1) .. 1]

boardEmpty :: Board
boardEmpty = boardFromList
    [ [tileEmpty, tileEmpty, tileEmpty, tileEmpty]
    , [tileEmpty, tileEmpty, tileEmpty, tileEmpty]
    , [tileEmpty, tileEmpty, tileEmpty, tileEmpty]
    , [tileEmpty, tileEmpty, tileEmpty, tileEmpty]
    ]

boardFromList :: [[Tile]] -> Board
boardFromList lines
    | length lines == boardSize && all ((== boardSize) . length) lines =
        A.array ((1, 1), (boardSize, boardSize)) $
        concat [ [((row, col), tile) | (col, tile) <- zip ascIndices tiles]
               | (row, tiles) <- zip descIndices lines
               ]
boardFromList _lines =
    error "boardFromList: invalid lines"

boardFringe :: [(Row, Column)]
boardFringe = nub $ [(row, col) | row <- [1..boardSize], col <- [1, boardSize]] ++
                    [(row, col) | col <- [1..boardSize], row <- [1, boardSize]]

boardEmptyTiles :: Board -> [(Row, Column)]
boardEmptyTiles board =
    [ (row, col)
    | (row, col) <- boardFringe, tileIsEmpty (board A.! (row, col))
    ]

------------------------------------------------------------------------
-- Game logic
------------------------------------------------------------------------

getMoveLines :: Board -> Move -> [[Tile]]
getMoveLines board (Horizontal, Normal) =
    [[board A.! (row, col) | col <- ascIndices]  | row <- ascIndices]
getMoveLines board (Horizontal, Reversed) =
    [[board A.! (row, col) | col <- descIndices] | row <- ascIndices]
getMoveLines board (Vertical,   Normal)   =
    [[board A.! (row, col) | row <- ascIndices]  | col <- ascIndices]
getMoveLines board (Vertical,   Reversed) =
    [[board A.! (row, col) | row <- descIndices] | col <- ascIndices]

writeMoveLines :: Move -> [[Tile]] -> Board
writeMoveLines (orientation, direction) lines = runST modify
  where
    index = case orientation of
        Horizontal -> \(outer, inner) -> (outer, inner)
        Vertical   -> \(outer, inner) -> (inner, outer)

    outerIndices = ascIndices
    
    innerIndices = case direction of
        Normal   -> ascIndices
        Reversed -> descIndices

    modify :: forall s. ST s Board
    modify = do
        mutBoard :: MutableBoard s <-
            A.newArray ((1, 1), (boardSize, boardSize)) tileEmpty
        forM_ (zip outerIndices lines) $ \(outer, line) ->
            forM_ (zip innerIndices line) $ \(inner, tile) ->
                A.writeArray mutBoard (index (outer, inner)) tile
        A.unsafeFreeze mutBoard

advanceMoveLines :: [[Tile]] -> Maybe [[Tile]]
advanceMoveLines lines = changed $ map (collapseLine . compactLine) $ lines
  where
    changed lines' =
        if or (map (uncurry lineChanged) (zip lines lines'))
        then Just lines' else Nothing

    lineChanged []       []         = False
    lineChanged (_ : _)  []         = False
    lineChanged []       (_ : _)    = error "advanceMoveLines: the impossible happened"
    lineChanged (t : ts) (t' : ts') =
        if t == t' then lineChanged ts ts' else True

    compactLine = filter (not . tileIsEmpty)

    collapseLine [] =
        []
    collapseLine ((tileIsFull -> Just s) : (tileIsFull -> Just s') : tiles) | s == s' =
        (tileFull (s + 1) :) $ collapseLine tiles
    collapseLine (tile : tiles) =
        (tile :) $ collapseLine tiles

spawnPiece :: RandomGen g => Board -> g -> (Board, g)
spawnPiece board gen0 =
    let (p, gen1) = randomR (0, 1) gen0
    in  (pickSpawn 0 p spawns, gen1)
  where
    spawns = toList $ possibleSpawns board

    pickSpawn _   _ []                      = error "spawnPiece: the impossible happened"
    pickSpawn _   _ [(_, board')]           = board'
    pickSpawn acc p ((q, board') : spawns') =
        let acc' = acc + q
        in if p < acc' then board' else pickSpawn acc' p spawns'

possibleMoves :: Board -> [(Move, Board)]
possibleMoves board = do
    move <- [moveLeft, moveRight, moveDown, moveUp]
    Just lines <- return $ advanceMoveLines (getMoveLines board move)
    return $ (move, writeMoveLines move lines)

possibleSpawns :: Board -> NonEmpty (Double, Board)
possibleSpawns board =
    case boards of
        []       -> error "possibleSpawns: cannot spawn"
        (b : bs) -> b :| bs
  where
    emptyTiles      = boardEmptyTiles board
    numEmptyTiles   = fromIntegral $ length emptyTiles
    highProbability = 0.9 / numEmptyTiles
    lowProbability  = 0.1 / numEmptyTiles

    boards = concat
        [ [ (highProbability, runST (modify ix (tileFull 0)))
          , (lowProbability,  runST (modify ix (tileFull 1)))
          ]
        | ix <- emptyTiles
        ]

    modify :: forall s. (Row, Column) -> Tile -> ST s Board
    modify tileIx tile = do
        mutBoard :: MutableBoard s <- A.thaw board
        A.writeArray mutBoard tileIx tile
        A.unsafeFreeze mutBoard

play :: RandomGen g => Board -> Move -> g -> Maybe (Board, g)
play board0 move gen0 =
     case advanceMoveLines (getMoveLines board0 move) of
         Nothing    -> Nothing
         Just lines -> Just $ spawnPiece (writeMoveLines move lines) gen0

------------------------------------------------------------------------
-- Board hashing
------------------------------------------------------------------------

-- boardToByteString :: Board -> ByteString
-- boardToByteString = undefined

-- byteStringToBoard :: ByteString -> Maybe Board
-- byteStringToBoard = undefined

------------------------------------------------------------------------
-- Board evaluation
------------------------------------------------------------------------

type BoardScore = Double

-- | TODO carry the possible moves and the empty tiles here.
type BoardEvaluator = Board -> BoardScore

resizeDouble :: (Double, Double) -> Double -> Double
resizeDouble (lo, hi) x = lo' + (x * ((hi' - lo') / (hi - lo)))
  where (lo', hi') = (-1, 1)

-- Simple evaluators
--------------------

-- | Evaluates the board between -1 and 1 depending on the spaces left
-- (lower score = less space left).
spacesLeftEvaluator :: BoardEvaluator
spacesLeftEvaluator =
    resizeDouble (0, maxSpacesLeft) . fromIntegral . length . boardEmptyTiles
  where
    maxSpacesLeft = fromIntegral $ length boardFringe

-- | -1 if the board doesn't have moves, 0 otherwise.
losingEvaluator :: BoardEvaluator
losingEvaluator board =
    if null (possibleMoves board) && null (boardEmptyTiles board)
    then -1 else 0

-- scoreEvaluator :: BoardEvaluator
-- scoreEvaluator = resizeDouble (0, maxScore) . fromIntegral . sumTiles
--   where
--     numTiles = fromIntegral (boardSize * boardSize)

--     maxScore = (2 ** (numTiles + 1)) * numTiles

--     sumTiles board = sum $ do
--         Just s <- tileIsFull <$> A.elems board
--         return $ 2 ^ s

compoundEvaluator :: [(Double, BoardEvaluator)] -> BoardEvaluator
compoundEvaluator evals board =
    sum $ [multiplier * eval board | (multiplier, eval) <- evals]

-- Combinators
--------------

spawnsEvaluator :: BoardEvaluator -> BoardEvaluator
spawnsEvaluator eval =
    sum . toList . fmap (\(p, board1) -> p * eval board1) . possibleSpawns

searchEvaluator :: BoardEvaluator -> BoardEvaluator
searchEvaluator eval = go 0
  where
    maxDepth :: Int
    maxDepth = 2

    go :: Int -> BoardEvaluator
    go depth board =
        -- If there are no possible moves we just return the score,
        -- hoping that the evaluator will penalize a losing board
        -- accordingly.
        if depth >= maxDepth || null nextBoards
        then boardScore
        else maximum $ map (spawnsEvaluator (go (depth + 1))) nextBoards
      where
        boardScore = eval board

        nextBoards = map snd $ possibleMoves board

-- Sample evaluators
--------------------

evaluator1 :: BoardEvaluator
evaluator1 =
    searchEvaluator
    (compoundEvaluator [(10000, losingEvaluator), (1, spacesLeftEvaluator)])

-- Actual solver
-----------------

solver :: BoardEvaluator -> Board -> Maybe Move
solver eval board0 = fmap fst $ headMay $ sortBy (comparing snd) $
    [ (move, negate (spawnsEvaluator eval board1))
    | (move, board1) <- possibleMoves board0
    ]

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

    drawTile tile =
        case tileIsFull tile of
            Nothing    -> putStr "  "
            Just score -> do
                when (score < 10) $ putStr " "
                putStr $ show score

------------------------------------------------------------------------
-- Sample boards
------------------------------------------------------------------------

parseBoard :: String -> Board
parseBoard =
    boardFromList . map (map parseTile) . chunksOf boardSize . splitOn "|"
  where
    parseTile x = if all isSpace x then tileEmpty else tileFull (read x)

almost1024 :: Board 
almost1024 = parseBoard $ concat $ intersperse "|" $
    [ "   |   |   |   "
    , "   |   |   |   "
    , "   |   |   | 6 "
    , " 9 | 8 | 7 | 6 "
    ]

------------------------------------------------------------------------
-- Main routine
------------------------------------------------------------------------

main :: IO ()
main = do
    gen <- getStdGen
    let (boardInit, gen') = spawnPiece boardEmpty gen
    go boardInit gen'
  where
    go board0 gen0 = do
        drawBoard board0
        putStrLn $ "Board score: " ++ show (evaluator1 board0)
        putStrLn $ showSuggestedMove board0
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

    showSuggestedMove board = case solver evaluator1 board of
        Nothing   -> "No suggested move."
        Just move -> ("Suggested move: " ++) $ case move of
            (Horizontal, Normal)   -> "left"
            (Horizontal, Reversed) -> "right"
            (Vertical,   Normal)   -> "down"
            (Vertical,   Reversed) -> "up"
