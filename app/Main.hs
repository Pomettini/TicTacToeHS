module Main where

import Data.List (maximumBy, minimumBy)
import Data.Maybe (isNothing, listToMaybe)
import Data.Ord (comparing)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Data types
data Player = X | O | Empty deriving (Eq, Show)

type Board = [[Player]]

data GameState = GameState
  { board :: Board,
    currentPlayer :: Player,
    winner :: Maybe Player
  }

-- Constants
windowSize :: Int
windowSize = 600

halfWindowSize :: Float
halfWindowSize = fromIntegral windowSize / 2

cellSize, halfCellSize, fourthCellSize :: Float
cellSize = 200
halfCellSize = cellSize / 2
fourthCellSize = cellSize / 4

window :: Display
window = InWindow "Tic Tac Toe" (windowSize, windowSize) (100, 100)

background, lineColor :: Color
background = white
lineColor = black

-- Initial state
initialState :: GameState
initialState =
  GameState
    { board = replicate 3 (replicate 3 Empty),
      currentPlayer = X,
      winner = Nothing
    }

-- Drawing
drawGrid :: Picture
drawGrid = pictures [verticalLines, horizontalLines]
  where
    verticalLines = pictures [line [(x, -halfWindowSize), (x, halfWindowSize)] | x <- [-halfCellSize, halfCellSize]]
    horizontalLines = pictures [line [(-halfWindowSize, y), (halfWindowSize, y)] | y <- [-halfCellSize, halfCellSize]]

drawX, drawO :: Picture
drawX = pictures [line [(-fourthCellSize, -fourthCellSize), (fourthCellSize, fourthCellSize)], line [(-fourthCellSize, fourthCellSize), (fourthCellSize, -fourthCellSize)]]
drawO = thickCircle fourthCellSize 10

drawCell :: Int -> Int -> Player -> Picture
drawCell row col player =
  translate x y $
    case player of
      X -> drawX
      O -> drawO
      Empty -> blank
  where
    x = fromIntegral col * cellSize - halfWindowSize + halfCellSize
    y = fromIntegral row * cellSize - halfWindowSize + halfCellSize

drawBoard :: Board -> Picture
drawBoard b = pictures [drawCell row col player | row <- [0 .. 2], col <- [0 .. 2], let player = b !! row !! col]

drawWinner :: Maybe Player -> Picture
drawWinner Nothing = blank
drawWinner (Just player) = scale 0.2 0.2 $
  text $
    case player of
      X -> "You win!"
      O -> "Computer wins!"
      Empty -> "It's a draw!"

draw :: GameState -> IO Picture
draw game = return $ pictures [drawGrid, drawBoard (board game), drawWinner (winner game)]

-- Input handling
handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) game
  | isValidMove row col game =
      if isWinner (currentPlayer game') (board game')
        then return $ initialState {winner = Just (currentPlayer game')} -- Set the winner
        else return game'
  | otherwise = return game
  where
    row = truncate $ (y + halfWindowSize) / cellSize
    col = truncate $ (x + halfWindowSize) / cellSize
    game' = makeMove row col game
handleInput _ game = return game

isValidMove :: Int -> Int -> GameState -> Bool
isValidMove row col game =
  isBlankCell (board game !! row !! col) && isNothing (winner game)

isBlankCell :: Player -> Bool
isBlankCell Empty = True
isBlankCell _ = False

makeMove :: Int -> Int -> GameState -> GameState
makeMove row col game =
  game
    { board = updateBoard row col (currentPlayer game) (board game),
      currentPlayer = nextPlayer
    }
  where
    nextPlayer = if currentPlayer game == X then O else X

updateBoard :: Int -> Int -> Player -> Board -> Board
updateBoard row col player board =
  [[if (i, j) == (row, col) then player else board !! i !! j | j <- [0 .. 2]] | i <- [0 .. 2]]

-- Game logic
isWinner :: Player -> Board -> Bool
isWinner player board =
  any (all (== player)) (rows ++ cols ++ [diag1, diag2])
  where
    rows = board
    cols = [[board !! i !! j | i <- [0 .. 2]] | j <- [0 .. 2]]
    diag1 = [board !! i !! i | i <- [0 .. 2]]
    diag2 = [board !! i !! (2 - i) | i <- [0 .. 2]]

makeAIMove :: GameState -> IO GameState
makeAIMove game = do
  let emptyCells = [(row, col) | row <- [0 .. 2], col <- [0 .. 2], isBlankCell (board game !! row !! col)]
  if null emptyCells
    then return $ initialState {winner = Just Empty}
    else do
      let (row, col) = findBestMove game
      return $ makeMove row col game

findBestMove :: GameState -> (Int, Int)
findBestMove game =
  case currentPlayer game of
    O -> snd $ maximumBy (comparing fst) [(minimax (makeMove row col game), (row, col)) | (row, col) <- emptyCells]
    _ -> snd $ minimumBy (comparing fst) [(minimax (makeMove row col game), (row, col)) | (row, col) <- emptyCells]
  where
    emptyCells = [(row, col) | row <- [0 .. 2], col <- [0 .. 2], isBlankCell (board game !! row !! col)]

minimax :: GameState -> Int
minimax game
  | isWinner X (board game) = -1
  | isWinner O (board game) = 1
  | allCellsOccupied (board game) = 0
  | currentPlayer game == O = maximum [minimax (makeMove row col game) | (row, col) <- emptyCells]
  | otherwise = minimum [minimax (makeMove row col game) | (row, col) <- emptyCells]
  where
    emptyCells = [(row, col) | row <- [0 .. 2], col <- [0 .. 2], isBlankCell (board game !! row !! col)]

findWinningMove :: Player -> GameState -> [(Int, Int)] -> Maybe (Int, Int)
findWinningMove player game emptyCells =
  listToMaybe [move | move <- emptyCells, isWinningMove player (makeMove (fst move) (snd move) game)]

isWinningMove :: Player -> GameState -> Bool
isWinningMove player game = isWinner player (board updatedGame)
  where
    updatedGame = makeMove 0 0 game

checkWinner :: GameState -> GameState
checkWinner game =
  if isNothing (winner game)
    then game {winner = determineWinner game}
    else game

determineWinner :: GameState -> Maybe Player
determineWinner game =
  if isWinner X (board game)
    then Just X
    else
      if isWinner O (board game)
        then Just O
        else
          if allCellsOccupied (board game)
            then Just Empty
            else Nothing

allCellsOccupied :: Board -> Bool
allCellsOccupied = all (all (/= Empty))

-- Main function
main :: IO ()
main = playIO window background 60 initialState draw handleInput update

-- Game loop update function
update :: Float -> GameState -> IO GameState
update _ game = do
  let updatedGame = checkWinner game
  case winner updatedGame of
    Just _ -> return updatedGame
    Nothing ->
      if currentPlayer updatedGame == O
        then makeAIMove updatedGame
        else return updatedGame
