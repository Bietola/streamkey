{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module StreamKeys (streamKeys) where

import Prelude hiding (log, break)
import Control.Applicative as A
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Control.Break

import Data.Maybe
import qualified Data.Text as T
import qualified Turtle as SH

unescapeDasherKey key
  | key == "<lt>" = "<"
  | key == "<gt>" = ">"
  | otherwise = key

parseDasherKeyEvent :: T.Text -> Maybe T.Text
parseDasherKeyEvent = listToMaybe . SH.match do
  "Key("
  key <- SH.selfless $ SH.many SH.dot
  ")"
  return $ unescapeDasherKey $ SH.fromString key

pressKey :: T.Text -> IO ()
pressKey key = SH.sh do
  let command = "xdotool key " <> key
  SH.shells command SH.empty

streamKeys :: IO ()
streamKeys =
  let liftIO = lift
      log = liftIO . print
      -- ignore = flip (>>) $ return ()
      -- lput = lift . put
      -- lget = lift get

  in forever $ flip evalStateT T.empty do
    line <- T.pack <$> liftIO getLine

    key <- maybe A.empty
             return
             (parseDasherKeyEvent line)

    log $ "Processing text from dasher: " <> key

    keyBuffer <- (<>key) <$> get
    log $ "Updated buffer: " <> keyBuffer

    if keyBuffer == "<!cr!>"
       then do
         put ""
         liftIO $ pressKey "Return"
    else if keyBuffer `T.isPrefixOf` "<!cr!>"
       then do
         log $ "Caching for <!cr!>: " <> keyBuffer
         put keyBuffer
    else do
      log $ "Pressing key: " <> key
      liftIO $ pressKey key
      put ""
