{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module RecordKeys
  (recordKeys
  -- DB
  , KeyEvent
  , parseRawKeyEvent
  , KeyMod
  ) where

import Data.Char as C
import GHC.Generics

import Data.Vector
import Data.Text as T
import Debug.Trace
import Data.Aeson
import Data.Aeson.Types
import Turtle

-- Corpus Processing Functions

data KeyMod = Alt | Ctrl | Super | Shift
  deriving (Show, Read, Generic)

instance FromJSON KeyMod

data KeyEvent = KeyEvent {
      scancode  :: Int
    , modifiers :: [KeyMod]
    } deriving Show

parseRawKeyMod jsonStr = do
  json <- eitherDecode $ fromString jsonStr

  case json of
    String modStr -> return $ read $ (\x -> trace x x) $ repr $ capitalize modStr
    _ -> Left "Json KeyMod should be a string."

  where capitalize txt = case T.uncons txt of
                           Nothing -> T.empty
                           Just (x, xs) -> T.cons (C.toUpper x) xs


parseRawKeyEvent :: String -> Either String KeyEvent
parseRawKeyEvent jsonStr = do
  json <- eitherDecode $ fromString jsonStr

  flip parseEither json $ \root -> do
    scancode  <- root .: "scan_code"
    (modifiers :: Array) <- root .: "modifiers"

    -- TODO: Find way to turn list of eithers into either of a list
    keymods <- toList $ parseRawKeyMod <$> modifiers

    return $ KeyEvent
      scancode
      (toList $ parseRawKeyMod <$> modifiers)
-- Terminal command

recordKeysRaw :: Shell Line
recordKeysRaw =
  -- TODO: Check if pystreamkey is installed on the system
  inshell "pystreamk-record" mempty

recordKeys :: IO ()
recordKeys = sh $ recordKeysRaw >>= echo
