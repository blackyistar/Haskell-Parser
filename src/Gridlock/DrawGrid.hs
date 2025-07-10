module Gridlock.DrawGrid where

import Gridlock.ColourSquares (square)
import Gridlock.Types ( Grid (rep), Cell (Empty, Filled), width )

-- Hint: These might come in handy :)
import Data.Map (elems)
import Data.Map qualified as Map

-- Converts a grid into string format
drawGrid :: Grid -> String
drawGrid g = "+" ++ replicate (width g) '-' ++ "+\n" ++ groupStrings (elems (Map.map renderCell (rep g))) (width g) ++ "+" ++ replicate (width g) '-' ++ "+"

-- Converts a cell into its string representation
renderCell :: Cell -> String
renderCell Empty = " "
renderCell (Filled c) = square c

-- Concatenates a list of strings, grouping x strings together on each line
-- Wraps each group with vertical lines
groupStrings :: [String] -> Int -> String
groupStrings [] _ = ""
groupStrings s x = "|" ++ concat (take x s) ++ "|\n" ++ groupStrings (drop x s) x
