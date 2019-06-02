module Game where

type Coords = (Int, Int)
data Input = MoveUp | MoveDown | MoveLeft | MoveRight deriving Show
data World = World {
                width :: Int,
                height :: Int,
                walls :: [Coords],
                blocks :: [Coords],
                holes :: [Coords],
                player :: Coords
                } deriving Show

-- Allows for updating of World state
instance Semigroup World where
    w1 <> w2 = w2 { holes = (holes w1)++(holes w2),
                    walls = (walls w1)++(walls w2),
                    blocks = (blocks w1)++(blocks w2),
                    player = if player w1 /= (-1,-1) then player w1 else player w2
                    }

data GameState = GameState {
                    current :: World,
                    blank :: World,
                    retries :: Int,
                    name :: String
                    } deriving Show

instance Semigroup GameState where
    g1 <> g2 = g1 { current = (current g1) <> (current g2) }

-- Converts World value to String
showWorld :: World -> String
showWorld world = concat [tile x y world : (if x == (width world) then "\n" else "")
                          | y <- [0..height world], x <- [0..width world]]
                            where 
                                tile x y world 
                                    | (x,y) == player world = '@'
                                    | (x,y) `elem` walls world = '#'
                                    | (x,y) `elem` blocks world = 'o'
                                    | (x,y) `elem` holes world = 'v'
                                    | otherwise = ' '

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
movePlayer input world = if isNotOverlapping world input then
                            Just $ world { 
                            player = updateCoords (player world) input--,
                            --moves = (moves world) + 1
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

-- Player is not overlapping if not moving into walls or stuck boulders
isNotOverlapping :: World -> Input -> Bool
isNotOverlapping world input =  not (proposedPlayer `elem` walls world ||
                                proposedPlayer `elem` holes world)
                                where proposedPlayer = updateCoords (player world) input
