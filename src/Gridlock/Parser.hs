{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant ==" #-}
module Gridlock.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set, fromList)
import qualified Data.Set as Set

import Data.Void
import Gridlock.DrawGrid
import Gridlock.Types ( Colour (..), GameRecord (..), Player, Grid (..), Coord, Cell (Filled, Empty) )

import Debug.Trace
import Data.Char (isDigit)

type Parser = Parsec Void String

{- | Parse a game into a GameRecord.

To run this function (either as part of your application, or for testing, you can use one of the parse* functions from the Megaparsec library, such as "parse" or "parseTest".)
-}
parseGame :: Parser GameRecord
parseGame = do
    string "GRIDLOCK\n"
    string "Players: "
    players <- parsePlayers
    string "Grid: "
    width <- read <$> some digitChar
    char 'x'
    height <- read <$> some digitChar
    string ".\n"
    string "Colours: "
    colours <- parseColours
    
    -- Construct an empty grid
    let emptyGrid = Grid
            { width = width
            , height = height
            , rep = Map.fromList [((x,y), Empty) | x <- [0..width-1], y <- [0..height-1]]
            }
    -- Construct a game record
    let game = GameRecord
            {   players = players
            ,   grids = [emptyGrid]
            ,   colours = colours
            }
    parseRounds game (head players)

-- Parse an empty line, consuming spaces and newlines
parseEmptyLine :: Parser ()
parseEmptyLine = do
    many (char ' ')
    many newline
    pure ()

-- Parse through all rounds of the game
parseRounds :: GameRecord -> Player -> Parser GameRecord
parseRounds g currentPlayer = do
    parseEmptyLine
    string currentPlayer -- Ensure it is the player's turn
    g2 <- parseMove g <|> parseWin g
    nextChar <- lookAhead (optional anySingle) -- Peek at the next character
    case nextChar of
        -- No more input left
        Nothing -> pure g2
        -- More input exists
        Just _  -> 
            if currentPlayer == head (players g) then
                parseRounds g2 (last $ players g)
            else
                parseRounds g2 (head $ players g)

-- Parse the end condition when the game is over
parseWin :: GameRecord -> Parser GameRecord
parseWin g = do
    string " cannot move." <|> string " wins!"
    if any usableColour (colours g) then -- There is a colour that could be used to fill an empty cell in the grid
        fail "Failed too early"
    else
        pure g
    where
        -- Check if the given colour can fill an empty cell in the grid
        usableColour x = any (colourSquare g x) (Map.keys $ rep $ last $ grids g) -- Returns true if x can be used to colour any cell in the grid

-- Parse a valid move
parseMove :: GameRecord -> Parser GameRecord
parseMove g = do
    string " plays "
    colour <- parseColour
    coord <- parseCoord g
    -- Construct the new grid after the move
    let newGrid = (last $ grids g) { rep = Map.insert coord (Filled colour) (rep $ last $ grids g) }
    -- Update the game record
    let newGame = g { grids = grids g ++ [newGrid]}

    -- Ensure the move is valid
    if not (colourSquare g colour coord) then
        fail "Move is not valid"
    else
        pure newGame

-- Check if a cell can be coloured with a given colour
colourSquare :: GameRecord -> Colour -> Coord -> Bool
colourSquare g colour coord =
    if Set.notMember colour (colours g) then -- colour is not in the list of available colours
        False
    else if Map.lookup coord (rep $ last $ grids g) /= Just Empty then -- cell is already coloured
        False
    else if Map.lookup (fst coord, snd coord + 1) (rep $ last $ grids g) == Just (Filled colour) ||
        Map.lookup (fst coord, snd coord - 1) (rep $ last $ grids g) == Just (Filled colour) ||
        Map.lookup (fst coord + 1, snd coord) (rep $ last $ grids g) == Just (Filled colour) ||
        Map.lookup (fst coord - 1, snd coord) (rep $ last $ grids g)== Just (Filled colour) then
         -- colour is the same as that of an adjacent cell
        False
    else
        True

-- Parse a valid coordinate (x,y)
parseCoord :: GameRecord -> Parser Coord
parseCoord g = do
    char '('
    x <- read <$> some digitChar
    char ','
    y <- read <$> some digitChar
    string ").\n"

    -- Ensure coordinates are within bounds
    if x > width (head $ grids g) - 1 || y > height (head $ grids g) - 1 then
        fail "Coordinates out of bounds"
    else
        pure (x, y)

-- Parse two player names seperated by a comma
parsePlayers :: Parser [Player]
parsePlayers = do
    player1 <- takeWhile1P Nothing (/= ',')
    string ", "
    player2 <- takeWhile1P Nothing (/= '.')
    string ".\n"
    pure [player1, player2]

-- Parse one or more colours into a set
parseColours :: Parser (Set Colour)
parseColours = Set.fromList <$> some parseColour

-- Parse a single colour
parseColour :: Parser Colour
parseColour = do
    colour <- choice [red, green, yellow, blue, magenta, cyan]
    string ", " <|> string ".\n" <|> string " at "
    pure colour
    where
        red = string "red" >> pure Red
        green = string "green" >> pure Green
        yellow = string "yellow" >> pure Yellow
        blue = string "blue" >> pure Blue
        magenta = string "magenta" >> pure Magenta
        cyan = string "cyan" >> pure Cyan
