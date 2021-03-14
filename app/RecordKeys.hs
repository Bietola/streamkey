{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module RecordKeys
  (recordKeys
  -- DB
  , KeyEvent
  , parseRawKeyEvent
  , KeyMod
  ) where

import Data.Char as C
import GHC.Generics

import Data.Vector as V
import Data.Text as T
import Debug.Trace
import Data.Aeson
import Data.Aeson.Types
import Turtle hiding (Parser)

-- Corpus Processing Functions

data KeyMod = Alt | Ctrl | Super | Shift
  deriving (Show, Read)

data KeyEvent = KeyEvent {
      scancode  :: Int
    , modifiers :: V.Vector KeyMod
    } deriving Show

parseRawKeyMod :: Value -> Parser KeyMod
parseRawKeyMod = withText "" $ \rawMod ->
  return $ read $ repr $ capitalize rawMod
  where capitalize txt = case T.uncons txt of
                             Nothing -> T.empty
                             Just (x, xs) -> T.cons (C.toUpper x) xs

parseRawKeyEvent :: Value -> Parser KeyEvent
parseRawKeyEvent = do
  withObject "" $ \root -> do
    scancode  <- root .: "scan_code"
    modifiers <- root .: "modifiers" >>= V.mapM parseRawKeyMod

    return KeyEvent{..}

recordKeysRaw :: Shell Line
recordKeysRaw =
  -- TODO: Check if pystreamkey is installed on the system
  inshell "pystreamk-record" mempty

recordKeys :: IO ()
recordKeys = sh $ recordKeysRaw >>= echo
