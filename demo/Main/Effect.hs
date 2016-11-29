module Main.Effect where

import Rebase.Prelude
import Strelka.ResponseBuilder (ResponseBuilder)
import qualified Rebase.Data.HashSet as A
import qualified Rebase.Data.HashMap.Strict as B


newtype Effect a =
  Effect (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

type Env =
  (TVar Numbers, TVar Credentials)

type Numbers =
  HashSet Int

type Credentials =
  HashMap Text Text


addNumber :: Int -> Effect ()
addNumber x =
  Effect (ReaderT (\(numbers, _) -> atomically (modifyTVar' numbers (A.insert x))))

listNumbers :: Effect [Int]
listNumbers =
  Effect (ReaderT (\(numbers, _) -> atomically (readTVar numbers >>= return . A.toList)))

deleteNumber :: Int -> Effect ()
deleteNumber x =
  Effect (ReaderT (\(numbers, _) -> atomically (modifyTVar' numbers (A.delete x))))

getCredentials :: Effect Credentials
getCredentials =
  Effect (ReaderT (\(_, credentials) -> atomically (readTVar credentials)))

authorize :: Text -> Text -> Effect Bool
authorize username password =
  fmap (maybe False (== password) . B.lookup username) getCredentials

listCredentials :: Effect [(Text, Text)]
listCredentials =
  fmap B.toList getCredentials
