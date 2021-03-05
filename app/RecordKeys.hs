{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module RecordKeys (recordKeys) where

import Prelude hiding (log)

import Turtle
import Text.Printf

recordKeys :: IO ()
recordKeys = sh do
  key <- recordKeysRaw
  echo key

recordKeysRaw :: Shell Line
recordKeysRaw = do
  -- TODO: Check if pystreamkey is installed on the system
  inshell "pystreamk-record" empty
