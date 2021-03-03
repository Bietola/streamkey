{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module RecordKeys (recordKeys) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Graphics.X11 as X

recordKeys :: IO ()
recordKeys = do
  dsp <- X.openDisplay ""
  print "Display opened"

  let whiteColor = whitePixel dsp (X.defaultScreen dsp)
  let blackColor = blackPixel dsp (X.defaultScreen dsp)

  win <- X.createSimpleWindow dsp (X.defaultRootWindow dsp) 0 0 200 100 0 blackColor blackColor

  X.selectInput dsp win X.keyPressMask

  X.mapWindow dsp win

  gc <- X.createGC dsp win

  X.setForeground dsp gc whiteColor

  X.allocaXEvent \eptr -> forever do
    print "Waiting for event..."
    X.nextEvent dsp eptr

    print "Got it!"
    eventType <- X.get_EventType eptr
    print eventType
