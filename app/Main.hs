{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Printf as T

import Options.Applicative

import Turtle

import StreamKeys
import RecordKeys

-- Get unicode escape code of given Text
encodeUnicode16 :: Text -> [Text]
encodeUnicode16 = T.foldl (\s c -> s ++ escapeChar c) []
  where
    escapeChar c
        | 'a' <= c && c <= 'z' = [T.singleton c]
        | otherwise =
            [T.pack $ T.printf "U%04x" (fromEnum c)]

-- `Record` keys or `Stream` them from stdin into keyboard events
data SubCommand = Record | Stream

-- For parsing arguments
argParser :: ParserInfo SubCommand
argParser = 
  info
    (helper <*> parser)
    (progDesc "Your 1 stop program to type with neither hands, nor eyes")

  where
    -- Two subcommands are registered: `record` and `stream`
    parser = subparser
      ( command
          "record"
          (info (pure Record)
            (progDesc "Record keypresses to later be processed into a Dasher corpus"))
     <> command
          "stream"
          (info (pure Stream)
            (progDesc "Stream keypresses using xdotool"))
      )

-- Just launch the argument parser and execute the appropriate subcommand
main :: IO ()
main = do
  subcmd <- execParser argParser

  case subcmd of
    Record -> recordKeys
    Stream -> streamKeys
