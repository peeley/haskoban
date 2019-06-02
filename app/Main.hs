{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.Directory

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    args <- getArgs
    let tArgs = map T.pack args
    parseArgs tArgs
      
getLevelDirectory :: IO T.Text
getLevelDirectory = do
    homeDirectory <- T.pack <$> getHomeDirectory
    let levelDirectory = homeDirectory <> "/.haskoban/levels/"
    return levelDirectory

parseArgs :: [T.Text] -> IO ()
parseArgs [] = do
    levelFile <- getLevelDirectory >>= pickRandomLevel 
    state <- loadState levelFile
    gameLoop state
parseArgs ("-l" : levelName : _) = playLevel levelName
parseArgs ("--level" : levelName : _) = playLevel levelName
parseArgs _ = helpMessage

playLevel :: T.Text -> IO ()
playLevel levelName = do
    levelFolder <- getLevelDirectory
    let levelFile = levelFolder <> levelName
    state <- loadState levelFile
    gameLoop state

helpMessage :: IO ()
helpMessage = do
    TIO.putStrLn "usage: haskoban [option] [arg]"
    TIO.putStrLn "Options: "
    TIO.putStrLn "-h, --help:\tDisplay this message."
    TIO.putStrLn "-l, --level [ARG]:\tPlay specific level, specified \
             \ by filename in the levels file."

