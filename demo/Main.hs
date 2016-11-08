module Main where

import Rebase.Prelude
import qualified Main.RequestParsers as A
import qualified Main.ResponseBuilders as B
import qualified Main.Effect as C
import qualified Router.WAI as D
import qualified Rebase.Data.HashSet as E
import qualified Rebase.Data.HashMap.Strict as F

main =
  do
    env <- newEnv
    D.routerServer 3000 (runEffect env) A.top
  where
    runEffect env (C.Effect impl) =
      fmap Right (runReaderT impl env)
    newEnv =
      atomically ((,) <$> newTVar E.empty <*> newTVar credentials)
      where
        credentials =
          F.fromList [("user1", "password1"), ("user2", "password2")]
