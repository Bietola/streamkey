{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe

import Options.Applicative

import Turtle

import StreamKeys
import RecordKeys

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

-- <su-tab>/<m-tab>/<c-tab>/<s-tab>/<f-tab>

-- main :: IO ()
-- main = sh $ runMaybeT $ do
--   keycode <- lift $ repr <$> stdin

--   let kcParser = has $ do "Key("; c <- chars; ")"; return c
--   key <- case match kcParser keycode of
--     [key] -> return key
--     _ -> mzero

--   liftIO $ print $ format ("pressing: "%s) key

data SubCommand = Record | Stream

argParser :: ParserInfo SubCommand
argParser = 
  info
    (helper <*> parser)
    (progDesc "Your 1 stop program to type with neither hands, nor eyes")

  where
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

-- TODO/TEST
main :: IO ()
main = do
  subcmd <- execParser argParser

  case subcmd of
    Record -> recordKeys
    Stream -> streamKeys
