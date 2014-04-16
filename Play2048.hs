{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Prelude                               hiding (lines)

import           Control.Applicative                   ((<*>))
import           Control.Monad                         (forM_, when, mzero, liftM, forM)
import           Control.Monad.ST                      (ST, runST, stToIO)
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Maybe             (runMaybeT)
import           Data.Char                             (isSpace)
import           Data.Functor                          ((<$>))
import qualified Data.HashTable.ST.Basic               as HT
import           Data.List                             (intercalate, sortBy, intersperse)
import           Data.List.NonEmpty                    (NonEmpty(..), toList)
import           Data.List.Split                       (splitOn, chunksOf)
import           Data.Maybe                            (catMaybes)
import           Data.Ord                              (comparing)
import           Data.STRef                            (newSTRef, modifySTRef', STRef, readSTRef)
import           Safe                                  (headMay)
import           System.Environment                    (getArgs)
import           System.IO                             (hSetBuffering, stdin, BufferMode(NoBuffering))
import qualified System.Random.MWC                     as MWC

import           Tile                                  (Tile, Score)
import qualified Tile
import           Board                                 (Board, Row, Column)
import qualified Board                                 as Board

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
-- Game logic
------------------------------------------------------------------------

getMoveLines :: Board -> Move -> [[Tile]]
getMoveLines board (Horizontal, Normal) =
    [[Board.read board (row, col) | col <- Board.ascIndices]  | row <- Board.ascIndices]
getMoveLines board (Horizontal, Reversed) =
    [[Board.read board (row, col) | col <- Board.descIndices] | row <- Board.ascIndices]
getMoveLines board (Vertical,   Normal)   =
    [[Board.read board (row, col) | row <- Board.ascIndices]  | col <- Board.ascIndices]
getMoveLines board (Vertical,   Reversed) =
    [[Board.read board (row, col) | row <- Board.descIndices] | col <- Board.ascIndices]

writeMoveLines :: Move -> [[Tile]] -> Board
writeMoveLines (orientation, direction) lines = runST modify
  where
    index = case orientation of
        Horizontal -> \(outer, inner) -> (outer, inner)
        Vertical   -> \(outer, inner) -> (inner, outer)

    outerIndices = Board.ascIndices
    
    innerIndices = case direction of
        Normal   -> Board.ascIndices
        Reversed -> Board.descIndices

    modify :: forall s. ST s Board
    modify = do
        mutBoard :: Board.Mutable s <- Board.thaw Board.empty 
        forM_ (zip outerIndices lines) $ \(outer, line) ->
            forM_ (zip innerIndices line) $ \(inner, tile) ->
                Board.write mutBoard (index (outer, inner)) tile
        Board.unsafeFreeze mutBoard

advanceMoveLines :: [[Tile]] -> Maybe [[Tile]]
advanceMoveLines lines =
    changed $ map (map Tile.full . collapseLine . compactLine) $ lines
  where
    changed lines' =
        if or (map (uncurry lineChanged) (zip lines lines'))
        then Just lines' else Nothing

    lineChanged []       []         = False
    lineChanged (_ : _)  []         = False
    lineChanged []       (_ : _)    = error "advanceMoveLines: the impossible happened"
    lineChanged (t : ts) (t' : ts') =
        if t == t' then lineChanged ts ts' else True

    compactLine = catMaybes . map Tile.isFull

    collapseLine [] =
        []
    collapseLine (s : s' : tiles) | s == s' =
        ((s + 1) :) $ collapseLine tiles
    collapseLine (tile : tiles) =
        (tile :) $ collapseLine tiles

spawnPiece :: Board -> MWC.GenST s -> ST s Board
spawnPiece board gen = do
    p <- MWC.uniformR (0, 1) gen
    return $ pickSpawn 0 p spawns
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
    emptyTiles      = Board.emptyTiles board
    numEmptyTiles   = fromIntegral $ length emptyTiles
    highProbability = 0.9 / numEmptyTiles
    lowProbability  = 0.1 / numEmptyTiles

    boards = concat
        [ [ (highProbability, runST (modify ix (Tile.full 0)))
          , (lowProbability,  runST (modify ix (Tile.full 1)))
          ]
        | ix <- emptyTiles
        ]

    modify :: forall s. (Row, Column) -> Tile -> ST s Board
    modify tileIx tile = do
        mutBoard :: Board.Mutable s <- Board.thaw board
        Board.write mutBoard tileIx tile
        Board.unsafeFreeze mutBoard

play :: MWC.GenST s -> Board -> Move -> ST s (Maybe Board)
play gen board0 move =
     case advanceMoveLines (getMoveLines board0 move) of
         Nothing    -> return $ Nothing
         Just lines -> Just <$> spawnPiece (writeMoveLines move lines) gen

------------------------------------------------------------------------
-- Board evaluation
------------------------------------------------------------------------

type BoardScore = Double

-- | TODO carry the possible moves and the empty tiles here.
type BoardEvaluator m = Board -> m BoardScore

resizeDouble :: (Double, Double) -> Double -> Double
resizeDouble (lo, hi) x = lo' + (x * ((hi' - lo') / (hi - lo)))
  where (lo', hi') = (-1, 1)

-- Simple evaluators
--------------------

-- | Evaluates the board between -1 and 1 depending on the spaces left
-- (lower score = less space left).
spacesLeftEvaluator :: Monad m => BoardEvaluator m
spacesLeftEvaluator =
    return . resizeDouble (0, maxSpacesLeft) . fromIntegral . length . Board.emptyTiles
  where
    maxSpacesLeft = fromIntegral $ length Board.fringe

smoothnessEvaluator :: Monad m => BoardEvaluator m
smoothnessEvaluator board = do
    let horizLines = getMoveLines board moveLeft
    let vertLines  = getMoveLines board moveDown
    return $ negate $ resizeDouble (0, fromIntegral maxRoughness)
           $ fromIntegral $ sum
           $ map (lineSmoothness . catMaybes . map Tile.isFull)
           $ horizLines ++ vertLines
  where
    maxRoughness =
        let pairsAndLeftover = uncurry (+) $ Board.size `divMod` 2
        in  pairsAndLeftover * 2 * (pairsAndLeftover * fromIntegral Tile.maxScore)

    lineSmoothness :: [Score] -> Int
    lineSmoothness []  = 0
    lineSmoothness [s] = fromIntegral s
    lineSmoothness (s : s' : tiles) =
        fromIntegral (abs (s - s')) + lineSmoothness tiles

-- | -1 if the board doesn't have moves, 0 otherwise.
losingEvaluator :: Monad m => BoardEvaluator m
losingEvaluator board = return $
    if null (possibleMoves board) && null (Board.emptyTiles board)
    then -1 else 0

-- scoreEvaluator :: BoardEvaluator
-- scoreEvaluator = resizeDouble (0, maxScore) . fromIntegral . sumTiles
--   where
--     numTiles = fromIntegral (Board.size * Board.size)

--     maxScore = (2 ** (numTiles + 1)) * numTiles

--     sumTiles board = sum $ do
--         Just s <- tileIsFull <$> A.elems board
--         return $ 2 ^ s

compoundEvaluator :: Monad m => [(Double, BoardEvaluator m)] -> BoardEvaluator m
compoundEvaluator evals board =
    sum `liftM` mapM (\(multiplier, eval) -> (multiplier *) `liftM` eval board) evals

-- Combinators
--------------

spawnsEvaluator :: Monad m => BoardEvaluator m -> BoardEvaluator m
spawnsEvaluator eval board = do
    let spawns = toList $ possibleSpawns board
    sum `liftM` mapM (\(p, board1) -> (p *) `liftM` eval board1) spawns

type Depth = Int

data SearchState s = SearchState
    { ssTable     :: HT.HashTable s (Board, Depth) BoardScore
    , ssTableSize :: STRef s Integer
    , ssLookups   :: STRef s Integer
    }

newSearchState :: ST s (SearchState s)
newSearchState =
    SearchState <$> HT.new <*> newSTRef 0 <*> newSTRef 0

searchEvaluator
    :: forall s. BoardEvaluator (ST s)
    -> SearchState s
    -> BoardEvaluator (ST s)
searchEvaluator eval searchState = go 0
  where
    maxDepth :: Int
    maxDepth = 2

    -- TODO shall we lookup or get the score if we have reached the
    -- maximum depth?
    go :: Depth -> BoardEvaluator (ST s)
    go depth board = do
        modifySTRef' (ssLookups searchState) (+ 1)
        mbScore <- HT.lookup (ssTable searchState) (board, depth)
        case mbScore of
            Just score -> return score
            Nothing -> do
                score <-
                    -- If there are no possible moves we just return the
                    -- score, hoping that the evaluator will penalize a
                    -- losing board accordingly.
                    if depth >= maxDepth || null nextBoards
                    then eval board
                    else maximum <$> mapM (spawnsEvaluator (go (depth + 1))) nextBoards
                modifySTRef' (ssTableSize searchState) (+ 1)
                HT.insert (ssTable searchState) (board, depth) score
                return score
      where
        nextBoards = map snd $ possibleMoves board

-- Sample evaluators
--------------------

evaluator1 :: Monad m => BoardEvaluator m
evaluator1 =
    compoundEvaluator [ (10000, losingEvaluator)
                      , (10, smoothnessEvaluator)
                      , (1, spacesLeftEvaluator)
                      ]

-- Actual solver
-----------------

solver :: Monad m => BoardEvaluator m -> Board -> m (Maybe Move)
solver eval board0 = do
    moves <- forM (possibleMoves board0) $ \(move, board1) -> do
        score <- spawnsEvaluator eval board1
        return (move, negate score)
    return $ fmap fst $ headMay $ sortBy (comparing snd) $ moves

------------------------------------------------------------------------
-- Drawing
------------------------------------------------------------------------

drawBoard :: Board -> IO ()
drawBoard board = do
    forM_ Board.descIndices $ \row -> do
        drawBorder      
        forM_ Board.ascIndices $ \col -> do
            putStr "|"
            drawTile $ Board.read board (row, col)
        putStrLn "|"
    drawBorder
  where
    drawBorder = do
        putStr "+"
        putStr $ intercalate "-" $ replicate Board.size "--"
        putStrLn "+"

    drawTile tile =
        case Tile.isFull tile of
            Nothing    -> putStr "  "
            Just score -> do
                when (score < 10) $ putStr " "
                putStr $ show score

------------------------------------------------------------------------
-- Sample boards
------------------------------------------------------------------------

parseBoard :: String -> Board
parseBoard =
    Board.fromList . map (map parseTile) . chunksOf Board.size . splitOn "|"
  where
    parseTile x = if all isSpace x then Tile.empty else Tile.full (read x)

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

mainSolver :: IO (Board -> IO (Maybe Move))
mainSolver = do
    searchState <- stToIO newSearchState
    return $ \board -> do
        mbMove <- stToIO $ solver (searchEvaluator evaluator1 searchState) board
        lookups <- stToIO $ readSTRef $ ssLookups searchState
        tableSize <- stToIO $ readSTRef $ ssTableSize searchState
        putStrLn $ "Table size: " ++ show tableSize
        putStrLn $ "Misses ratio: " ++ show (100 * fi tableSize / fi lookups :: Double) ++ "%"
        return mbMove
  where
    fi = fromIntegral

mainAuto :: IO ()
mainAuto = do
    gen       <- stToIO MWC.create
    solve     <- mainSolver
    boardInit <- stToIO $ spawnPiece Board.empty gen
    go solve gen boardInit
  where
    go solve gen board0 = do
        drawBoard board0
        mbMove <- solve board0
        case mbMove of
            Nothing   -> drawBoard board0
            Just move -> do
                 Just board1 <- stToIO $ play gen board0 move
                 go solve gen board1

mainPlay :: IO ()
mainPlay = do
    hSetBuffering stdin NoBuffering

    gen       <- stToIO MWC.create
    solve     <- mainSolver
    boardInit <- stToIO $ spawnPiece Board.empty gen
    go solve gen boardInit
  where
    go solve gen board0 = do
        drawBoard board0
        showSuggestedMove solve board0
        board1 <- getAndApplyMove gen board0
        go solve gen board1

    getAndApplyMove gen board0 = do
        move <- getMove
        putStrLn ""
        putStrLn ""
        putStrLn ""
        mbBoard1 <- stToIO $ play gen board0 move
        case mbBoard1 of
            Nothing     -> getAndApplyMove gen board0
            Just board1 -> return board1

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

    showSuggestedMove solve board = do
        mbMove <- solve board
        putStrLn $ case mbMove of
            Nothing   -> "No suggested move."
            Just move -> ("Suggested move: " ++) $ case move of
                (Horizontal, Normal)   -> "left"
                (Horizontal, Reversed) -> "right"
                (Vertical,   Normal)   -> "down"
                (Vertical,   Reversed) -> "up"

main :: IO ()
main = do
    [mode] <- getArgs
    case mode of
        "auto" -> mainAuto
        "play" -> mainPlay
        _      -> putStrLn "Wrong usage"
