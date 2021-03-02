{-# LANGUAGE OverloadedStrings #-}

module RecordKeys (recordKeys) where

import Turtle

recordKeys = do
  rawPress <- inshell "python3 pyscripts/record_kps.py" stdin

  liftIO $ print ("rawpress: " <> rawPress)
