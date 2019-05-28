module Main where

import Lib
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    levelFileName <- ("levels/" ++) <$> pickRandomLevel
    world <- loadWorld levelFileName
    gameLoop world
        
