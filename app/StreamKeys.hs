{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module StreamKeys (streamKeys) where

import Prelude hiding (log, break)
import Control.Applicative as A
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Data.Functor ((<&>))
import Data.Either
import Control.Break
import GHC.Exts

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Text.Printf as T
import qualified Data.Vector as V

import qualified Turtle as Sh

import qualified XDoCom

data BufferMode = FillBuffer | ConsumeBuffer | IgnoreBuffer
  deriving (Eq, Show)

data DasherContext =
  DasherContext { history :: [Text]
                , bsAmount :: [Int]
                , bufferMode :: BufferMode
                , buffer :: V.Vector Text }

defDasherContext =
  DasherContext { history = []
                , bsAmount = []
                , bufferMode = IgnoreBuffer
                , buffer = V.empty}

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
  | key == "<esc>" = "Escape"
  | key == " " = "KP_Space"
  | otherwise = head $ encodeUnicode16 key

parseDasherKeyEvent :: T.Text -> Maybe T.Text
parseDasherKeyEvent = listToMaybe . Sh.match do
  "Key("
  key <- Sh.selfless $ Sh.many Sh.dot
  ")"
  return $ Sh.fromString key

isDasherEscapeSeq :: Text -> Bool
isDasherEscapeSeq =
  not . null . Sh.match do { "<"; Sh.selfless $ Sh.star Sh.anyChar; ">" }

handleDasherKeypress :: DasherContext -> Text -> IO DasherContext
handleDasherKeypress context key =
  let

    pressDasherKey :: DasherContext -> Text -> IO DasherContext
    pressDasherKey context key = do
      case bufferMode context of
        m | m `elem` [IgnoreBuffer, ConsumeBuffer] -> do
          -- TODO: Handle error using better version of commented out code below
          -- -- TODO: Simplify after proper logging
          -- -- either
          --   -- (T.printf "Error parsing escape sequence: %s")
          --   print
          --   (XDoCom.execute >=> either print print)
          --   (XDoCom.parse escapeSeq)
          XDoCom.pressKey key
          return context
        FillBuffer ->
          return context

    undoHistory :: DasherContext -> DasherContext
    undoHistory context =
      case bufferMode context of
        IgnoreBuffer -> context { history = tail $ history context }
        FillBuffer -> 
          if V.null $ buffer context
            then
              -- TODO: Consider this instead?: context { bufferMode = IgnoreBuffer }
              error "Buffer should not be empty here!"
            else
              context { buffer = V.tail $ buffer context }
        ConsumeBuffer -> 
          error "History should not be undone while consuming buffer!"

    addToHistory :: DasherContext -> Text -> DasherContext
    addToHistory context key =
      case bufferMode context of
        m | m `elem` [IgnoreBuffer,  ConsumeBuffer] ->
          context { history = key : history context }
        FillBuffer ->
          context { buffer = V.snoc (buffer context) key }

    changeBufferMode :: DasherContext -> Text -> DasherContext
    changeBufferMode context key = context { bufferMode = newMode }
      where
        oldMode = bufferMode context
        newMode = case oldMode of

          IgnoreBuffer ->
              -- TODO: Handle other bufferMode changing keys
              if key `elem` ["<esc>", "TODO_FILLME"] then
                FillBuffer
              else
                oldMode

          FillBuffer ->
            -- TODO: Handle abort and quit
            if key `elem` ["<wbf>", "TODO_FILLME"] then
              ConsumeBuffer
            else
              oldMode

          ConsumeBuffer ->
            if V.null $ buffer context
              then
                IgnoreBuffer
              else
                oldMode

  in do

  T.printf "[%s] Pressing key: %s\n" (show $ bufferMode context) key

  let hist = history context
      bsAmnt = bsAmount context
      lastKey = head hist

  if key == "<bs>" then do
    T.printf "[WARNING] Backspace not supported yet!\n"
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
        context' <- pressDasherKey context "BackSpace"
        -- NB. `removeLast` for going back removing whole escape seq
        return $ undoHistory context'{ bsAmount = -(T.length lastKey) + 2 : bsAmnt }
      else do
        pressDasherKey context "BackSpace" <&> undoHistory

    -- press  key normally
    else do
      context' <- pressDasherKey context $ dasherKeyCodeToXCode key
      let context'' = addToHistory context' key
      let context''' = changeBufferMode context'' key

      return context'''

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

    nextEvent :: DasherContext -> IO (Text, DasherContext)
    nextEvent context = do
      let buf = buffer context
          bufMode = bufferMode context
      
      if bufMode == ConsumeBuffer then do
        if V.null buf then do
          T.printf "Buffer empty, accepting new events from stdin\n"
          nextEvent context{ bufferMode = IgnoreBuffer }
        else
          return (V.head buf, context{ buffer = V.tail buf })
          
      else do
        rawEvent <- T.pack <$> getLine
        return (rawEvent, context)

    streamKeysRec :: DasherContext -> IO ()
    streamKeysRec context = do
      (rawEvent, context') <- nextEvent context

      maybeParseKeyEvent rawEvent
        -- on parse fail
        (streamKeysRec context') 
        -- on parse ok
        \key -> do
          T.printf "[MainMode] Processing text from dasher: %s\n" key

          if key == "<"
            then do
              print "Initiating escape"
              streamWithEscape context' T.empty

          else do
            handleDasherKeypress context' key >>= streamKeysRec

    streamWithEscape :: DasherContext -> Text -> IO ()
    streamWithEscape context escapeSeq = do
      (rawEvent, context') <- nextEvent context

      maybeParseKeyEvent rawEvent
        -- on parse fail
        (streamWithEscape context' escapeSeq)
        -- on parse ok
        \key -> do
          T.printf "[EscapeMode] Processing text from dasher: %s\n" key

          if key == "<udo>" then
            if T.null escapeSeq then do
              -- exit escape mode
              T.printf "Aborting escape because of user undo!\n"
              streamKeysRec context'
            else do
              -- Remove last character of escape seq
              let newEscSeq = T.take (T.length escapeSeq - 1) escapeSeq
              T.printf "[EscapeMode] Undoing (%s)\n" newEscSeq
              streamWithEscape context' newEscSeq

          else if key == ">" then do
            let escapeSeq' = T.concat ["<", escapeSeq, ">"]

            T.printf "Finalizing escape: %s\n" escapeSeq'
            handleDasherKeypress context' escapeSeq' >>= streamKeysRec

          else do
            T.printf "Caching keys: %s\n" key
            streamWithEscape context' $ escapeSeq <> key
