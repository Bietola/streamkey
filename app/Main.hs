{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe

import Options.Applicative

import Turtle

import StreamKeys
import RecordKeys

-- Convert special keycodes to xdotool keycodes
keycode2xdotool :: String -> Maybe String
keycode2xdotool keycode =
  case keycode of
    "<cr>"    -> Just "Return"
    "<tab>"   -> Just "Tab"
    "<super>" -> Just "Super"
    "<ctrl>"  -> Just "Ctrl"
    "<meta>"  -> Just "Meta"
    "<bs>"    -> Just "BackSpace"
    _         -> Nothing

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
