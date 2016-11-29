module Main where

import Prelude
import qualified Strelka.Demo.Route as A
import qualified Strelka.Demo.ResponseBuilder as B
import qualified Strelka.Demo.Effect as C
import qualified Strelka.WAI as D
import qualified Data.HashSet as E
import qualified Data.HashMap.Strict as F

main =
  do
    env <- newEnv
    D.strelkaServer 3000 (runEffect env) A.top
  where
    runEffect env (C.Effect impl) =
      fmap Right (runReaderT impl env)
    newEnv =
      atomically ((,) <$> newTVar E.empty <*> newTVar credentials)
      where
        credentials =
          F.fromList [("user1", "password1"), ("user2", "password2")]
