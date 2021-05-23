{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module XDoCom where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception hiding (catch)
import Control.Monad.Catch
import Control.Foldl as Fold
import Turtle (s, (%))
import qualified Turtle as Sh

debugMode = True

data XDoCom = Key Text | Nil

type Result a = Either String a

execute :: XDoCom -> IO (Result String)
execute (Key key) = let
  lines = Sh.single . flip (Sh.fold @Sh.Shell) Fold.list
  try =
    Right . show <$> lines do
      Sh.when debugMode $ Sh.printf ("xdotool key "%s%"\n") key
      let command = "xdotool key " <> key
      Sh.inshell command Sh.empty
  in try `catch`
    \(SomeException e) -> return $ Left $ show e

-- TODO: WIP
execute _ = undefined

parse :: IsString s => s -> Result XDoCom
parse = undefined

pressKey :: T.Text -> IO (Result String)
pressKey = execute . Key
