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
import Data.Either
import Control.Break
import GHC.Exts

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Text.Printf as T
import qualified Turtle as SH

import qualified XDoCom

-- Get unicode escape code of given Text
encodeUnicode16 :: Text -> [Text]
encodeUnicode16 = T.foldl (\s c -> s ++ escapeChar c) []
  where
    escapeChar c = [T.pack $ T.printf "U%04x" (fromEnum c)]

-- TODO
dasherKeyCodeToXCode :: T.Text -> T.Text
dasherKeyCodeToXCode key
  | key == "<lt>" = "<"
  | key == "<gt>" = ">"
  | key == "<bs>" = "BackSpace"
  | key == "<cr>" = "Return"
  | key == "<tab>" = "Tab"
  | key == " " = "KP_Space"
  | otherwise = head $ encodeUnicode16 key

parseDasherKeyEvent :: T.Text -> Maybe T.Text
parseDasherKeyEvent = listToMaybe . SH.match do
  "Key("
  key <- SH.selfless $ SH.many SH.dot
  ")"
  return $ SH.fromString key

streamKeys :: IO ()
streamKeys =
  streamKeysRec
  where
    -- TODO: Remove the need for this with logging?
    maybeParseKeyEvent :: Text -> IO () -> (Text -> IO ()) -> IO ()
    maybeParseKeyEvent rawLine onFail onOk = 
      case parseDasherKeyEvent rawLine of
        Nothing -> do
          T.printf "Ignoring invalid key event: %s\n" rawLine
          onFail

        Just key -> onOk key

    streamKeysRec :: IO ()
    streamKeysRec = do
      line <- T.pack <$> getLine

      maybeParseKeyEvent line
        streamKeysRec $
        \key -> do
          T.printf "Processing text from dasher: %s\n" key

          if key == "<"
            then do
              print "Initiating escape"
              streamWithEscape T.empty

          else do
            T.printf "Pressing key: %s\n" key
            XDoCom.pressKey $ dasherKeyCodeToXCode key
            streamKeysRec

    streamWithEscape :: Text -> IO ()
    streamWithEscape buffer = do
      line <- T.pack <$> getLine

      maybeParseKeyEvent line
        (streamWithEscape buffer) $
        \key -> do
          T.printf "Processing text from dasher: %s\n" key

          if key == ">"
            then do
              let finalBuffer = T.concat ["<", buffer, ">"]

              T.printf "Finalizing escape: %s" finalBuffer

              -- TODO: Bring this back instead of following
              -- -- TODO: Simplify after proper logging
              -- either
              --   -- (T.printf "Error parsing escape sequence: %s")
              --   print
              --   (XDoCom.execute >=> either print print)
              --   (XDoCom.parse buffer)
              XDoCom.pressKey $ dasherKeyCodeToXCode finalBuffer

              streamKeysRec

          else do
            T.printf "Caching keys: %s\n" key
            streamWithEscape $ buffer <> key
