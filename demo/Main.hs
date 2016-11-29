module Main where

import Prelude
import qualified Strelka.Demo.Route as A
import qualified Strelka.Demo.Resource as B
import qualified Strelka.Demo.Effect as C
import qualified Strelka.WAI as D


main =
  do
    resource <- B.new
    D.strelkaServer 3000 (fmap Right . flip C.run resource) A.top
