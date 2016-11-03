module Main where

import Rebase.Prelude
import qualified Main.RequestParsers as A
import qualified Main.ResponseBuilders as B
import qualified Main.Effect as C
import qualified Router.WAI as D


main =
  D.routeServer 80 C.run A.top
