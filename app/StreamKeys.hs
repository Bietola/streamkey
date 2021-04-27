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
import qualified Turtle as Sh

import qualified XDoCom

data DasherContext =
  DasherContext { history :: [Text]
                , bsAmount :: [Int] }

defDasherContext =
  DasherContext { history = []
                , bsAmount = [] }

-- Get unicode escape code of given Text
encodeUnicode16 :: Text -> [Text]
encodeUnicode16 = T.foldl (\s c -> s ++ escapeChar c) []
  where
    escapeChar c = [T.pack $ T.printf "U%04x" (fromEnum c)]

-- TODO
dasherKeyCodeToXCode :: T.Text -> T.Text
dasherKeyCodeToXCode key
  | key == "<udo>" = error "Should not be handled by this..."
  | key == "<lt>" = "<"
  | key == "<gt>" = ">"
  | key == "<bs>" = "BackSpace"
  | key == "<cr>" = "Return"
  | key == "<tab>" = "Tab"
  | key == " " = "KP_Space"
  | otherwise = head $ encodeUnicode16 key

parseDasherKeyEvent :: T.Text -> Maybe T.Text
parseDasherKeyEvent = listToMaybe . Sh.match do
  "Key("
  key <- Sh.selfless $ Sh.many Sh.dot
  ")"
  return $ Sh.fromString key

-- TODO: Implement
isDasherEscapeSeq :: Text -> Bool
isDasherEscapeSeq = not . null . Sh.match do { "<"; Sh.star Sh.anyChar; ">" }

pressDasherKey :: DasherContext -> Text -> IO DasherContext
pressDasherKey context key = do
  T.printf "Pressing key: %s\n" key

  -- TODO: Handle error using better version of commented out code below
  -- -- TODO: Simplify after proper logging
  -- -- either
  --   -- (T.printf "Error parsing escape sequence: %s")
  --   print
  --   (XDoCom.execute >=> either print print)
  --   (XDoCom.parse escapeSeq)
  let hist = history context
      bsAmnt = bsAmount context
      lastKey = head hist

  if key == "<bs>" then do
    T.printf "Backspace not supported yet!\n"
    return context

  else if key == "<udo>"
    -- Handle dasher undo
    then
      if not $ null bsAmnt then do
        T.printf "Processing abnormal space amount specifier: %s\n" (show bsAmnt)

        if head  bsAmnt <= 0 then do
          -- Negative amount means don't produce spaces until amount reaches 0
          let newBSAmnt = if head bsAmnt == 0
                            then tail bsAmnt
                            else (head bsAmnt + 1) : tail bsAmnt

          -- NB. Not going back in history
          return context{ bsAmount = newBSAmnt }

        else
          -- Positive means repeat this number of spaces and remove amount entry
          -- TODO: Implement
          -- NB: Remember to go back in history
          undefined

      -- Add abnormal backspace amounts for escape codes
      else if isDasherEscapeSeq lastKey then do
        -- Only go back once for the whole escape sequence
        void $ XDoCom.pressKey "BackSpace"
        return context{ history = tail hist -- Going back remove hole escape seq
                      , bsAmount = -(T.length lastKey) + 2 : bsAmount context }
      else do
        void $ XDoCom.pressKey "BackSpace"
        return context{ history = tail hist }

    -- press  key normally
    else do
      void $ XDoCom.pressKey $ dasherKeyCodeToXCode key
      return context{ history = key : hist }

streamKeys :: IO ()
streamKeys =
  streamKeysRec defDasherContext
  where
    -- TODO: Remove the need for this with logging?
    maybeParseKeyEvent :: Text -> IO () -> (Text -> IO ()) -> IO ()
    maybeParseKeyEvent rawLine onFail onOk = 
      case parseDasherKeyEvent rawLine of
        Nothing -> do
          T.printf "Ignoring invalid key event: %s (U%s)\n" rawLine (show $ encodeUnicode16 rawLine)
          onFail

        Just key -> onOk key

    streamKeysRec :: DasherContext -> IO ()
    streamKeysRec context = do
      line <- T.pack <$> getLine

      maybeParseKeyEvent line
        -- on parse fail
        (streamKeysRec context) 
        -- on parse ok
        \key -> do
          T.printf "[MainMode] Processing text from dasher: %s\n" key

          if key == "<"
            then do
              print "Initiating escape"
              streamWithEscape context T.empty

          else do
            pressDasherKey context key >>= streamKeysRec

    streamWithEscape :: DasherContext -> Text -> IO ()
    streamWithEscape context escapeSeq = do
      line <- T.pack <$> getLine

      maybeParseKeyEvent line
        -- on parse fail
        (streamWithEscape context escapeSeq)
        -- on parse ok
        \key -> do
          T.printf "[EscapeMode] Processing text from dasher: %s\n" key

          if key == "<udo>" then
            if T.null escapeSeq then do
              -- exit escape mode
              T.printf "Aborting escape because of user undo!\n"
              streamKeysRec context
            else do
              -- Remove last character of escape seq
              let newEscSeq = T.take (T.length escapeSeq - 1) escapeSeq
              T.printf "[EscapeMode] Undoing (%s)\n" newEscSeq
              streamWithEscape context newEscSeq

          else if key == ">" then do
            let escapeSeq' = T.concat ["<", escapeSeq, ">"]

            T.printf "Finalizing escape: %s\n" escapeSeq'
            pressDasherKey context escapeSeq' >>= streamKeysRec

          else do
            T.printf "Caching keys: %s\n" key
            streamWithEscape context $ escapeSeq <> key
