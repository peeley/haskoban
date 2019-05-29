{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import System.IO
import System.Environment

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    args <- getArgs
    parseArgs args
       
parseArgs :: [String] -> IO ()
parseArgs [] = do
    levelFileName <- ("levels/" ++) <$> pickRandomLevel
    world <- loadWorld levelFileName
    gameLoop world
parseArgs ("-l" : levelName : _) = playLevel levelName
parseArgs ("--level" : levelName : _) = playLevel levelName
parseArgs _ = helpMessage

playLevel :: String -> IO ()
playLevel levelName = do
    let levelFileName = ("levels/" ++) levelName
    world <- loadWorld levelFileName
    gameLoop world

helpMessage :: IO ()
helpMessage = do
    putStrLn "usage: haskoban [option] [arg]"
    putStrLn "Options: "
    putStrLn "-h, --help:\tDisplay this message."
    putStrLn "-l, --level [ARG]:\tPlay specific level, specified \
             \ by filename in the levels file."

