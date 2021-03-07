{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module RecordKeys (recordKeys) where

import Turtle

recordKeys :: IO ()
recordKeys = sh do
  line <- inshell "pystreamk-record" empty
  echo line

recordKeysRaw :: Shell Line
recordKeysRaw = do
  -- TODO: Check if pystreamkey is installed on the system
  inshell "pystreamk-record" empty
