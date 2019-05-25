module Main where

import Lib

main :: IO ()
main = do
    levelFileName <- ("levels/" ++) <$> pickRandomLevel
    world <- loadWorld levelFileName
    gameLoop world
        
