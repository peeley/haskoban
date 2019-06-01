{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.List
import System.IO
import System.Console.ANSI
import System.Directory
import System.Random
import Game

-- Main game loop - updates game until 'finished' condition is met.
gameLoop :: World -> IO ()
gameLoop world =
    if isFinished world then do
        clearScreen
        setCursorPosition 0 0
        setTermColor Green
        putStrLn "\n\nCONGRATULATIONS, You Won!"
        _ <- getLine
        putStr ""
    else do
        clearScreen
        setCursorPosition 0 0
        setTermColor Yellow
        putStrLn $ "[" ++ name world ++ "]"
        setTermColor Blue
        putStrLn $ "Slots: " ++ (show . length . holes) world
        putStrLn $ "Crates: " ++ (show . length . blocks) world
        putStrLn $ "Moves: " ++ (show . moves) world ++ "\n"
        setTermColor White
        putStrLn $ showWorld world
        userInput <- getInput 
        let newWorld = updateWorld userInput world
        case newWorld of
            Just w -> gameLoop w
            otherwise -> gameLoop world

setTermColor :: Color -> IO ()
setTermColor color = do
    setSGR [ SetConsoleIntensity NormalIntensity ]
    setSGR [ SetColor Foreground Vivid color ]


-- Player can move up/left/right/down
getInput :: IO Input
getInput = do
    input <- getChar
    case input of
        'w' -> return MoveUp
        'a' -> return MoveLeft
        's' -> return MoveDown
        'd' -> return MoveRight
        otherwise -> getInput

-- Loads file at filename into World value
loadWorld :: FilePath -> IO World
loadWorld fileName = do
    fileHandle <- openFile fileName ReadMode
    fileContents <- hGetContents fileHandle
    let (width:height:level) = lines fileContents
    let defaultWorld = World fileName (read width) (read height) [] [] [] (-1,-1) 0
    return $ loadRows defaultWorld 0 level

-- Loads each row tile by tile, until width is reached
loadRows :: World -> Int -> [String] -> World
loadRows world _ [] = world
loadRows world n (x:xs) = (loadRow world (0,n) x) <> loadRows world (n+1) xs

loadRow :: World -> Coords -> String -> World
loadRow world _ [] = world
loadRow world (x,y) (char:left) = addTile world (x,y) char <> 
                                    loadRow world (x+1,y) left

-- Adds character to world at current coords
-- Currently parses Nethack tileset
addTile :: World -> Coords -> Char -> World
addTile world coords char
    | char == '|' || char == '-' = world { walls = coords : (walls world)}
    | char == '^' = world { holes = coords : (holes world)}
    | char == '0' = world { blocks = coords : (blocks world)}
    | char == '@' = world { player = coords}
    | otherwise = world

-- Picks a random level file from the local levels/  directory
pickRandomLevel :: FilePath -> IO FilePath
pickRandomLevel baseDirectory = do
    localLevelFiles <- filter (\ x -> x /= "." && x /= "..") <$> getDirectoryContents baseDirectory
    randomIndex <- randomRIO (0, (length localLevelFiles)-1)
    return $ baseDirectory ++ localLevelFiles!!randomIndex
