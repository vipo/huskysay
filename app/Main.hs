module Main where

import System.Environment
import Types
import Defaults
import Lib
import Parser

main :: IO ()
main = 
  do args <- getArgs
     (font,text) <- parseArgs args
     mapM_ putStrLn $ render text font

parseArgs :: [String] -> IO (Font,String)
parseArgs (text:[]) = return (defaultFont,text)
parseArgs (font:filler:text:[]) = 
  do content <- readFile font
     return (parseBdfFont content
                          (head filler)
            ,text)
parseArgs _ = 
  error $
  concat ["Invalid parameters. "
         ,"Valid parameters are: [font_file filler_char] text_to_render"]
