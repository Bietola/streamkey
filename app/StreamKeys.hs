{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module StreamKeys (streamKeys) where

import Turtle

pressKey :: Text -> Shell ()
pressKey key = do
  let command = "xdotool key " <> key
  shells command empty

streamKeys :: IO ()
streamKeys = sh do
  key <- lineToText <$> stdin

  case textToLine $ "Pressing: " <> key of
    Just t -> echo t

  pressKey key
