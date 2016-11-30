module Strelka.Demo.ResponseBuilder where

import Prelude
import Strelka.ResponseBuilder
import qualified Strelka.ResponseBody as A


notFoundInHTML :: ResponseBuilder
notFoundInHTML =
  notFoundStatus <> html "<h1>404 Not Found</h1>"

notFoundInText :: ResponseBuilder
notFoundInText =
  notFoundStatus <> text "404 Not Found"

badRequestInHTML :: ResponseBuilder
badRequestInHTML =
  badRequestStatus <> html "<h1>400 Bad Request</h1>"

badRequestInText :: ResponseBuilder
badRequestInText =
  badRequestStatus <> text "400 Bad Request"

listCredentialsAsJSON :: [(Text, Text)] -> ResponseBuilder
listCredentialsAsJSON credentials =
  json body
  where
    body =
      "[" <> (mconcat . intersperse "," . map credentialBody) credentials <> "]"
      where
        credentialBody (username, password) =
          "{" <> "\"username\":" <> usernameBody <> "," <> "\"password\":" <> passwordBody <> "}"
          where
            usernameBody =
              "\"" <> A.text username <> "\""
            passwordBody =
              "\"" <> A.text password <> "\""

listCredentialsAsHTML :: [(Text, Text)] -> ResponseBuilder
listCredentialsAsHTML credentials =
  html body
  where
    body =
      "<ul>" <> foldMap credentialBody credentials <> "</ul>"
      where
        credentialBody (username, password) =
          "<li>" <> usernameBody <> ":" <> passwordBody <> "</li>"
          where
            usernameBody =
              "<b>" <> A.text username <> "</b>"
            passwordBody =
              A.text password

listCredentialsAsText :: [(Text, Text)] -> ResponseBuilder
listCredentialsAsText credentials =
  text body
  where
    body =
      (fold . intersperse ", " . map credentialBody) credentials
      where
        credentialBody (username, password) =
          A.text username <> ":" <> A.text password

listNumbersAsJSON :: [Int] -> ResponseBuilder
listNumbersAsJSON numbers =
  json body
  where
    body =
      fromString ("[" <> intercalate "," (map show numbers) <> "]")

listNumbersAsHTML :: [Int] -> ResponseBuilder
listNumbersAsHTML numbers =
  html body
  where
    body =
      fromString ("<ul>" <> foldMap (\x -> "<li>" <> show x <> "</li>") numbers <> "</ul>")

