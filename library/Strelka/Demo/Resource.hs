module Strelka.Demo.Resource where

import Prelude
import qualified Data.HashSet as A
import qualified Data.HashMap.Strict as B


type Resource =
  (TVar Numbers, TVar Credentials)

type Numbers =
  HashSet Int

type Credentials =
  HashMap Text Text

new :: IO Resource
new =
  atomically ((,) <$> newTVar A.empty <*> newTVar credentials)
  where
    credentials =
      B.fromList [("user1", "password1"), ("user2", "password2")]
