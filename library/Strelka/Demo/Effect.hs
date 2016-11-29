module Strelka.Demo.Effect where

import Prelude
import qualified Data.HashSet as A
import qualified Data.HashMap.Strict as B
import qualified Strelka.Demo.Resource as C


newtype Effect a =
  Effect (ReaderT C.Resource IO a)
  deriving (Functor, Applicative, Monad, MonadIO)


run :: Effect a -> C.Resource -> IO a
run (Effect reader) resource =
  runReaderT reader resource

addNumber :: Int -> Effect ()
addNumber x =
  Effect (ReaderT (\(numbers, _) -> atomically (modifyTVar' numbers (A.insert x))))

listNumbers :: Effect [Int]
listNumbers =
  Effect (ReaderT (\(numbers, _) -> atomically (readTVar numbers >>= return . A.toList)))

deleteNumber :: Int -> Effect ()
deleteNumber x =
  Effect (ReaderT (\(numbers, _) -> atomically (modifyTVar' numbers (A.delete x))))

getCredentials :: Effect C.Credentials
getCredentials =
  Effect (ReaderT (\(_, credentials) -> atomically (readTVar credentials)))

authorize :: Text -> Text -> Effect Bool
authorize username password =
  fmap (maybe False (== password) . B.lookup username) getCredentials

listCredentials :: Effect [(Text, Text)]
listCredentials =
  fmap B.toList getCredentials
