module Main where

import System.Environment

import Types
import Defaults
import Lib

main :: IO ()
main = do
  args <- getArgs
  (font, text) <- parseArgs args
  mapM_ putStrLn $ render text font

parseArgs :: [String] -> IO (Font, String)
parseArgs (text:[]) = return (defaultFont, text)
parseArgs (font:text:[]) = do
  content <- readFile font 
  return (parseFont content, text)
parseArgs _ = error $ concat ["Invalid parameters. ",
  "Valid parameters are: [font file] text_to_render"]