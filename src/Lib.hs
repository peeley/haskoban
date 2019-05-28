{-# LANGUAGE OverloadedStrings #-}
module Lib (
        pickRandomLevel,
        loadWorld,
        gameLoop
        )
        where

import Data.List
import System.IO
import System.Directory
import System.Random
import System.Console.ANSI

type Coords = (Int, Int)
data Input = MoveUp | MoveDown | MoveLeft | MoveRight deriving Show
data World = World {
                width :: Int,
                height :: Int,
                walls :: [Coords],
                blocks :: [Coords],
                holes :: [Coords],
                player :: Coords,
                moves :: Int
                } deriving Show

-- Allows for updating of World state
instance Semigroup World where
    w1 <> w2 = World { width = if width w1 /= -1 then width w1 else width w2,
                       height = if height w1 /= -1 then height w1 else height w2,
                       holes = (holes w1)++(holes w2),
                       walls = (walls w1)++(walls w2),
                       blocks = (blocks w1)++(blocks w2),
                       player = if player w1 /= (-1,-1) then player w1 else player w2,
                       moves = moves w1
                       }

-- Allows for empty world state before parsing level file
instance Monoid World where
    mempty = World (-1) (-1) [] [] [] (-1,-1) 0
    a `mappend` b = a <> b

showWorld :: World -> String
showWorld world = concat [tile x y world ++ (if x == (width world) then "\n" else "")
                          | y <- [0..height world], x <- [0..width world]]
                            where 
                                tile x y world 
                                    | (x,y) == player world = "@"
                                    | (x,y) `elem` walls world = "#"
                                    | (x,y) `elem` blocks world = "o"
                                    | (x,y) `elem` holes world = "v"
                                    | otherwise = " "

gameLoop :: World -> IO ()
gameLoop world =
    if isFinished world then do
        clearScreen
        setCursorPosition 0 0
        putStrLn $ showWorld world
        setSGR [ SetConsoleIntensity BoldIntensity]
        setSGR [ SetColor Foreground Vivid Green ]
        putStrLn "\n\nCONGRATULATIONS, You Won!"
    else do
        clearScreen
        setCursorPosition 0 0
        putStrLn $ "Buckets: " ++ (show . length . holes) world
        putStrLn $ "Moves: " ++ (show . moves) world
        putStrLn $ showWorld world
        userInput <- getInput 
        let newWorld = updateWorld userInput world
        case newWorld of
            Just w -> gameLoop w
            otherwise -> gameLoop world

updateWorld :: Input -> World -> Maybe World
updateWorld input world = Just world >>= 
                        (movePlayer input) >>=
                        (pushBlock input) >>=
                        fillHoles

-- Specified block can be pushed if it is not moving into wall or other block.
canPushBlock :: Coords -> Input -> World -> Bool
canPushBlock block input world = not (movedBlock `elem` (walls world) 
                                 || movedBlock `elem` (blocks world))
                                    where movedBlock = updateCoords block input

-- Updates world's holes record by removing overlapping blocks/holes
fillHoles :: World -> Maybe World
fillHoles world = Just world { holes = remainingHoles, blocks = remainingBlocks }
    where 
        remainingBlocks = [block | block <- blocks world, not (block `elem` holes world)]
        remainingHoles = [hole | hole <- holes world, not (hole `elem` blocks world)]

-- Player is pushing a block if they are occupying same tile.
playerIsPushing :: World -> Bool
playerIsPushing world = (player world) `elem` (blocks world)

-- Pushes a block by updating list of block coordinates
pushBlock :: Input -> World -> Maybe World
pushBlock input world = if playerIsPushing world then 
                            let block = player world 
                                pushed = 
                                    updateCoords block input : filter (/= block) (blocks world)
                            in if canPushBlock block input world then 
                                Just world { 
                                blocks = pushed
                                }
                            else
                                Nothing
                      else
                        Just world

-- Updates world by moving player
movePlayer :: Input -> World  -> Maybe World
movePlayer input world = if isValidInput world input then
                            Just $ world { 
                            player = updateCoords (player world) input,
                            moves = (moves world) + 1
                            }
                        else
                            Nothing

-- Updates coordinates according to move direction
updateCoords :: Coords -> Input -> Coords
updateCoords (x, y) MoveUp = (x, y-1)
updateCoords (x, y) MoveDown = (x, y+1)
updateCoords (x, y) MoveLeft = (x-1, y)
updateCoords (x, y) MoveRight = (x+1, y)

-- Game is over when no more holes need to be filled.
isFinished :: World -> Bool
isFinished world = length (holes world) == 0

-- If the player is moving inside bounds and not overlapping input is valid
isValidInput :: World -> Input -> Bool
isValidInput world input = insideBounds world input && isNotOverlapping world input

-- Player is not overlapping if not moving into walls or stuck boulders
isNotOverlapping :: World -> Input -> Bool
isNotOverlapping world input =  not (proposedPlayer `elem` walls world ||
                                proposedPlayer `elem` holes world)
                                where proposedPlayer = updateCoords (player world) input

-- Checks if player is within height/width of world
insideBounds :: World -> Input -> Bool
insideBounds world MoveRight = fst (player world) < (width world)
insideBounds world MoveLeft = fst (player world) > 0
insideBounds world MoveUp =  snd (player world) > 0
insideBounds world moveDown = snd (player world) < (height world)

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

loadWorld :: FilePath -> IO World
loadWorld fileName = do
    fileHandle <- openFile fileName ReadMode
    fileContents <- hGetContents fileHandle
    let (width:height:level) = lines fileContents
    let defaultWorld = World (read width) (read height) [] [] [] (-1,-1) 0
    return $ loadRows defaultWorld 0 level

loadRows :: World -> Int -> [String] -> World
loadRows world _ [] = world
loadRows world n (x:xs) = (loadTiles world (0,n) x) <> loadRows world (n+1) xs

loadTiles :: World -> Coords -> String -> World
loadTiles world _ [] = world
loadTiles world (x,y) (char:left) = addTile world (x,y) char <> 
                                    loadTiles world (x+1,y) left

addTile :: World -> Coords -> Char -> World
addTile world index char
    | char == '|' || char == '-' = world { walls = index : (walls world)}
    | char == '^' = world { holes = index : (holes world)}
    | char == '0' = world { blocks = index : (blocks world)}
    | char == '@' = world { player = index}
    | otherwise = world

pickRandomLevel :: IO FilePath
pickRandomLevel = do
    localLevelFiles <- filter (\ x -> x /= "." && x /= "..") <$> getDirectoryContents "levels"
    randomIndex <- randomRIO (0, (length localLevelFiles)-1)
    return $ localLevelFiles!!randomIndex
