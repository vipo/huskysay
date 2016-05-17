module Main where

import System.Environment

import Types
import Defaults
import Lib
import Parser

main :: IO ()
main = do
  args <- getArgs
  (font, text) <- parseArgs args
  --putStrLn $ show font
  putStrLn $ render text font

parseArgs :: [String] -> IO (Font, String)
parseArgs (text:[]) = return (defaultFont, text)
parseArgs (font:text:[]) = do
  content <- readFile font 
  return (parseBdfFont content, text)
parseArgs _ = error $ concat ["Invalid parameters. ",
  "Valid parameters are: [font file] text_to_render"]