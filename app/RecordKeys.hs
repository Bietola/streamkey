{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module RecordKeys (recordKeys) where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Bits
import Text.Printf

import Graphics.X11 as X
import Graphics.X11.Xlib.Extras as X

recordKeys :: IO ()
recordKeys = do
  dsp <- X.openDisplay ""
  printf "Display opened successfully\n"

  grabStatus <-
    X.grabKeyboard
      dsp
      (X.defaultRootWindow dsp)
      True
      X.grabModeAsync
      X.grabModeAsync
      X.currentTime
  
  case grabStatus of
    st
      | st == X.alreadyGrabbed -> error "Keyboard already grabbed"
      | st == X.grabNotViewable -> error "Grab window not viewable"
      | st == X.grabFrozen -> error "Keyboard is frozen"
      | st == X.grabInvalidTime -> error "Invalid grab time"

    _ -> printf "Keyboard grab successful!\n"

  X.allocaXEvent \eptr -> forever $ do
    X.nextEvent dsp eptr

    etype <- X.get_EventType eptr

    printf "%d\n" etype

    (inputFocus, _) <- X.getInputFocus dsp
    let emptyMask = 0
    X.sendEvent dsp inputFocus True (X.keyPressMask .|. X.keyReleaseMask) eptr
