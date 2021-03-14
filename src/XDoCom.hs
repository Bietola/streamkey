{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module XDoCom where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Control.Foldl as Fold
import qualified Turtle as Sh

data XDoCom = Key Text | Nil

type Result a = Either String a

execute :: XDoCom -> IO (Result String)
execute (Key key) = undefined <$> lines do
  let command = "xdotool key " <> key
  -- TODO/CC: Lean about exceptions and turn the one thrown
  Sh.inshell command Sh.empty
    -- TODO/CC: Figure out wtf this is...
    where lines = Sh.single $ flip fold Fold.list
execute _ = error "WIP"

parse :: IsString s => s -> Result XDoCom
parse = undefined

pressKey :: T.Text -> IO (Result String)
pressKey = execute . Key
