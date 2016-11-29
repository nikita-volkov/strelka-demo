module Strelka.Demo.JSONBytesBuilder where

import Prelude hiding (try)
import JSONBytesBuilder.Builder


listCredentials :: [(Text, Text)] -> Literal
listCredentials credentials =
  array (foldMap credentialsElement credentials)
  where
    credentialsElement (username, password) =
      element (object rows)
      where
        rows =
          row "username" (stringFromText username) <>
          row "password" (stringFromText password)

listNumbers :: [Int] -> Literal
listNumbers numbers =
  array (foldMap (element . numberFromInt) numbers)
