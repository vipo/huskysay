module Main where

import           Defaults
import           Lib
import           Parser
import           System.Environment
import           Types

main :: IO ()
main =
  do args <- getArgs
     (font,text) <- parseArgs args
     putStrLn $ render text font

parseArgs :: [String] -> IO (Font,String)
parseArgs [text] = return (defaultFont,text)
parseArgs [font, filler, text] =
  do content <- readFile font
     return (parseBdfFont content (head filler),text)
parseArgs _ =
  error ("Invalid parameters. " ++
     "Valid parameters are: [font_file filler_char] text_to_render")
