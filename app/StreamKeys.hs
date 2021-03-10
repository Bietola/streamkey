{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module StreamKeys (streamKeys) where

import Prelude hiding (log)
import Control.Applicative as A
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import Data.Maybe
import Data.Text as T
import Turtle as SH

unescapeDasherKey key
  | key == "<lt>" = "<"
  | key == "<gt>" = ">"
  | otherwise = key

parseDasherKeyEvent :: Text -> Maybe Text
parseDasherKeyEvent = listToMaybe . match do
  "Key("
  key <- SH.selfless $ SH.many SH.dot
  ")"
  return $ unescapeDasherKey $ fromString key

pressKey :: Text -> Shell ()
pressKey key = do
  let command = "xdotool key " <> key
  shells command SH.empty

log = liftIO . print

streamKeys :: IO ()
streamKeys = sh $ flip runStateT T.empty do
  line <- lift stdin

  key <- maybe A.empty
           return
           (parseDasherKeyEvent $ lineToText line)

  log $ "Processing text from dasher: " <> key

  keyBuffer <- (<>key) <$> get
  log $ "Updated buffer: " <> keyBuffer

  if keyBuffer == "<!cr!>"
     then do
       put ""
       lift $ pressKey "Return"
  else if keyBuffer `T.isPrefixOf` "<!cr!>"
     then do
       log $ "Caching for <!cr!>: " <> keyBuffer
       put keyBuffer
  else do
    log $ "Pressing key: " <> key
    lift $ pressKey key
    put ""
