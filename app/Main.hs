{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import System.IO
import System.Environment
import System.Directory

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    args <- getArgs
    parseArgs args
      
getLevelDirectory :: IO FilePath
getLevelDirectory = do
    homeDirectory <- getHomeDirectory
    let levelDirectory = homeDirectory ++ "/.haskoban/levels/"
    return levelDirectory

parseArgs :: [String] -> IO ()
parseArgs [] = do
    levelFile <- getLevelDirectory >>= pickRandomLevel 
    state <- loadState levelFile
    gameLoop state
parseArgs ("-l" : levelName : _) = playLevel levelName
parseArgs ("--level" : levelName : _) = playLevel levelName
parseArgs _ = helpMessage

playLevel :: FilePath -> IO ()
playLevel levelName = do
    levelFolder <- getLevelDirectory
    let levelFile = levelFolder ++ levelName
    state <- loadState levelFile
    gameLoop state

helpMessage :: IO ()
helpMessage = do
    putStrLn "usage: haskoban [option] [arg]"
    putStrLn "Options: "
    putStrLn "-h, --help:\tDisplay this message."
    putStrLn "-l, --level [ARG]:\tPlay specific level, specified \
             \ by filename in the levels file."

