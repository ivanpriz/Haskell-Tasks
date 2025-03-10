module GuessWhat where

-- import System.Random


processNumber :: Int -> Int -> IO ()
processNumber playerNum initNum
  | playerNum > initNum = putStrLn "Too big"
  | playerNum < initNum = putStrLn "Too small"
  | otherwise = putStrLn "Yep, that's the number!"

askForNum :: IO Int
askForNum = do
  putStr "Your number: "
  playerNumberStr <- getLine
  let playerNumber = read playerNumberStr :: Int
  return playerNumber

gameLoop :: Int -> IO ()
gameLoop initNum = do
    playerNumber <- askForNum
    processNumber playerNumber initNum
    let comp = compare initNum playerNumber
    if (comp == LT || comp == GT) then gameLoop initNum else return ()

playGuessGame :: IO ()
playGuessGame = do
  let number = 5  -- fst $ randomR (1::Int, 100::Int) $ mkStdGen 2021
  gameLoop number
