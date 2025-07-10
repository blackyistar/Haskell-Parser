-- This is the main entry point for your Gridlock application.

import Gridlock.DrawGrid
import Gridlock.Parser
import Gridlock.Types
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
    args <- getArgs  -- Get command-line arguments
    if null args then do
        putStrLn "No arguments provided"
    else do
        gridlock <- readFile (head args) -- Read the file from the first argument
        let result = parse parseGame "" gridlock
        -- Handle the result of parsing
        case result of
            Left error -> putStrLn $ errorBundlePretty error
            Right game -> mapM_ (putStrLn . drawGrid) (grids game)
