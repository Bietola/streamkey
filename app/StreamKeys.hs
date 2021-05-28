{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module StreamKeys (streamKeys) where

import Prelude hiding (log, break)
import Control.Applicative as A
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Either
import Control.Monad.Trans.State.Strict
import Control.Arrow
import Control.Exception
import Data.Functor ((<&>))
import Data.Either
import Data.List
import Data.List.Index
import Control.Break
import System.IO hiding (BufferMode, Buffer)
import GHC.Exts

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Text.Printf as T
import qualified Data.Vector as V
import Control.Lens
import qualified Control.Lens.Regex.Text as Re
import Control.Lens.Regex.Text (regex)
import qualified Control.Foldl as Fl
import qualified Turtle as Sh

import qualified XDoCom

data BufferMode = 
  FillBuffer |
  UseEscapeBuffer BufferMode |
  ConsumeBuffer BufferMode |
  IgnoreBuffer

  deriving (Eq, Show)

data DasherContext =
  DasherContext { history :: [Text]
                , bsAmount :: [Int]
                , bufferMode :: BufferMode
                , buffer :: V.Vector Text
                , escapeBuffer :: Text }

defDasherContext =
  DasherContext { history = []
                , bsAmount = []
                , bufferMode = IgnoreBuffer
                , buffer = V.empty
                , escapeBuffer = T.empty }

-- Get unicode escape code of given Text
encodeUnicode16 :: Text -> [Text]
encodeUnicode16 = T.foldl (\s c -> s ++ escapeChar c) []
  where
    escapeChar c = [T.pack $ T.printf "U%04x" (fromEnum c)]

-- TODO
dasherKeyCodeToXCode :: T.Text -> T.Text
dasherKeyCodeToXCode keycode =
  -- let (mods, sepNKey) = T.break ((||) <$> (=='+') <*> (=='-')) keycode -- TODO: Try alternative
  case keycode ^? [regex|<(?:([scam]+)([-+]))?(.+)>|] . index 0 . Re.groups of
    Just ["", "", key] -> specialKey2x key
    Just [mods, sep, key] ->
      T.intercalate "+" $ map modAbbr2x (T.unpack mods) ++ [key2x key]
        where key2x = case sep of
                        "+" -> specialKey2x
                        "-" -> normalKey2x
    Nothing -> normalKey2x keycode

  where
    -- Special keys are things like `cr`, `bs`, etc...
    specialKey2x :: T.Text -> T.Text
    specialKey2x key
      | key == "udo" = error "Should not be handled by this..."
      | key == "lt" = "less"
      | key == "gt" = "greater"
      | key == "bs" = "BackSpace"
      | key == "cr" = "Return"
      | key == "tab" = "Tab"
      | key == "esc" = "Escape"
      | otherwise = error $ "Unexistent special dasher key: " ++ T.unpack key
      
    normalKey2x key
      | key == " " = "KP_Space"
      | otherwise = head $ encodeUnicode16 key

    modAbbr2x abbr
      | abbr == 's' = "shift"
      | abbr == 'c' = "ctrl"
      | abbr == 'a' = "alt"
      | abbr == 'm' = "super"

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

        -- m | m `elem` [IgnoreBuffer, ConsumeBuffer] -> do
        --   -- Ignore buffer control sequences when consuming the buffer
        --   -- TODO: Add other sequences
        --   if bufferMode context == ConsumeBuffer && key `elem` ["<bfi>", "<bfw>", "<bfq>"]
        --     then
        --       T.printf "[ConsumeBuffer] Ignoring buffer control seq: %s\n" key
        --     else do
        --       -- TODO: Handle error using better version of commented out code below
        --       -- -- TODO: Simplify after proper logging
        --       -- -- either
        --       --   -- (T.printf "Error parsing escape sequence: %s")
        --       --   print
        --       --   (XDoCom.execute >=> either print print)
        --       --   (XDoCom.parse escapeSeq)
        --       T.printf "Pressing key: %s\n" key
        --       void $ XDoCom.pressKey $ dasherKeyCodeToXCode key

        (ConsumeBuffer _) | key `elem` ["<bfi>", "<bfw>", "<bfq>"] -> do
          T.printf "[ConsumeBuffer] Ignoring buffer control seq: %s\n" key
          return context

        FillBuffer ->
          return context

        -- TODO: Use https://wiki.haskell.org/MultiCase when available
        (ConsumeBuffer _) -> doPress
        IgnoreBuffer      -> doPress
        where 
          doPress = do
            -- TODO: Handle error using better version of commented out code below
            -- -- TODO: Simplify after proper logging
            -- -- either
            --   -- (T.printf "Error parsing escape sequence: %s")
            --   print
            --   (XDoCom.execute >=> either print print)
            --   (XDoCom.parse escapeSeq)
            T.printf "Pressing key: %s\n" key
            void $ XDoCom.pressKey $ dasherKeyCodeToXCode key
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
              context { buffer = V.init $ buffer context }
        (ConsumeBuffer _) -> 
          error "History should not be undone while consuming buffer!"

    addToHistory :: DasherContext -> Text -> IO DasherContext
    addToHistory context key =
      case bufferMode context of
        IgnoreBuffer ->
          -- Extend history
          return $ context { history = key : history context }
        FillBuffer -> do
          -- Extend history *and* buffer
          let newBuffer = V.snoc (buffer context) key
          T.printf "[FillBuffer] Buffer: %s\n" (show newBuffer)
          return $ context { history = key : history context
                           , buffer = newBuffer }
        -- NB. `ConsumeBuffer` does not fill history because `FillBuffer already does`
        -- WARNING: This only works if no undo functionality is implemented in `ConsumeBuffer` mode!
        (ConsumeBuffer _) -> return context

    changeBufferMode :: DasherContext -> Text -> DasherContext
    changeBufferMode context key = context { bufferMode = newMode }
      where
        oldMode = bufferMode context
        newMode = case oldMode of

          IgnoreBuffer ->
              -- TODO: Handle other bufferMode changing keys
              if key `elem` ["<bfi>", "TODO_FILLME"] then
                FillBuffer
              else
                oldMode

          FillBuffer | key == "<bfq>" ->
            -- TODO: Handle abort and quit
            ConsumeBuffer IgnoreBuffer
          FillBuffer | key == "<bfw>" ->
            ConsumeBuffer FillBuffer
          FillBuffer ->
              oldMode

          (ConsumeBuffer nextMode) ->
            if V.null $ buffer context
              then
                nextMode
              else
                oldMode

  in do

  T.printf "[%s] Handling key: %s\n" (show $ bufferMode context) key

  let hist    = history context
      bsAmnt  = bsAmount context
      lastKey = head hist
      bufMod  = bufferMode context

  if key == "<udo>"
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
        context' <-
          if lastKey /= "<bs>" 
             then pressDasherKey context "<bs>"
             else do
               T.printf "[ERROR] CAN'T UNDO BACKSPACE! ALL UNDOS IGNORED UNTIL NON-BS KEY IS PRESSED!\n"
               return context -- Don't undo backspace with a backspace...

        -- NB. `removeLast` for going back removing whole escape seq
        return $ undoHistory context'{ bsAmount = -(T.length lastKey) + 2 : bsAmnt }

      else do
        pressDasherKey context "<bs>" <&> undoHistory

    -- press  key normally
    else do
      context' <- pressDasherKey context key
      context'' <- addToHistory context' key
      let context''' = changeBufferMode context'' key

      return context'''

streamKeys :: IO ()
streamKeys = do
  -- Intro message
  putStrLn "WARNING: DO NOT CTRL-C THIS PROGRAM TO CLOSE IT. Terminate it by closing dasher, so that bookkeeping things can be done correctly."

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

    -- (mkey, context) <- runStateT nextEvent context
    maybeNextEvent :: StateT DasherContext IO (Maybe Text)
    maybeNextEvent = do
      context <- get

      let buf = buffer context
          bufMode = bufferMode context

      case bufMode of

        (ConsumeBuffer nextMode) | V.null buf -> do
          lift $ T.printf "Buffer empty, accepting new events from stdin\n"
          put $ context { bufferMode = nextMode }
          maybeNextEvent

        (ConsumeBuffer _) -> do
          put $ context { buffer = V.tail buf }
          return $ Just $ V.head buf

        _ -> do

          -- Get raw event from stdin
          line <- lift $ try @IOError getLine
          rawEvent <-
            lift $ case line of
              Right line -> return $ T.pack line
              Left e -> do
                -- Write history to file before exiting
                lines <- fmap Sh.lineToText <$> Sh.reduce Fl.list (Sh.input "assets/history")

                let linesN = length lines

                let lastHistNumStr =
                      flip execState T.empty $ 
                        forM lines $
                          (^? [regex|----\s+(\d+)|] . index 0 . Re.group 0) >>> mapM_ put

                when (T.null lastHistNumStr) $
                  error "Couldn't retrieve last history number"

                let histNum = read @Int (T.unpack lastHistNumStr) + 1

                T.printf "Writing history #[%d]\n" histNum
                
                outh <- openFile "assets/history" AppendMode
                hPutStrLn outh $ "---- " <> show histNum
                T.hPutStrLn outh $ T.concat $ reverse $ history context
                hClose outh
                throw e

          -- Parse it (migth fail)
          case parseDasherKeyEvent rawEvent of
            Nothing -> do
              lift $ T.printf "Ignoring invalid key event: %s (U%s)\n" rawEvent (show $ encodeUnicode16 rawEvent)
              return Nothing
            Just key -> return $ Just key

    streamKeysRec :: DasherContext -> IO ()
    streamKeysRec context = do
      (maybeKey, context') <- runStateT maybeNextEvent context

      case maybeKey of
        -- on parse fail
        Nothing -> streamKeysRec context' 

        -- on parse ok
        Just key -> do
          let bufMode = bufferMode   context'
              escBuf  = escapeBuffer context'

          T.printf "[%s] Processing text from dasher: %s\n"(show bufMode) key

          case bufferMode context' of

            -- Escape Mode
            (UseEscapeBuffer nextBufMode) ->
              if key == "<udo>" then
                if T.null escBuf then do
                  -- exit escape mode
                  T.printf "Aborting escape because of user undo!\n"
                  streamKeysRec context' { bufferMode = nextBufMode }
                else do
                  -- Remove last character of escape seq
                  let newEscSeq = T.take (T.length escBuf - 1) escBuf
                  T.printf "[EscapeMode] Undoing (%s)\n" newEscSeq
                  streamKeysRec $ context' { escapeBuffer = newEscSeq }

              else if key == ">" then do
                let escSeq = T.concat ["<", escBuf, ">"]
                T.printf "Finalizing escape: %s\n" escSeq
                handleDasherKeypress context' escSeq >>= streamKeysRec

              else do
                T.printf "Caching keys: %s\n" key
                streamKeysRec $ context' { escapeBuffer = escBuf <> key }

            -- Other modes are handled within `handleDasherKeypress`
            _ -> 
              if key == "<"
                then do
                  streamKeysRec $ context' { bufferMode = UseEscapeBuffer IgnoreBuffer
                                           , escapeBuffer = T.empty }
                else do
                  handleDasherKeypress context' key >>= streamKeysRec
