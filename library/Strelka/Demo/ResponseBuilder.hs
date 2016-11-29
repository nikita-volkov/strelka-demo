module Strelka.Demo.ResponseBuilder where

import Prelude
import Strelka.ResponseBuilder
import qualified Strelka.ResponseBody as A
import qualified Strelka.Demo.JSONBytesBuilder as B
import qualified JSONBytesBuilder.ByteString.Builder as C
import qualified JSONBytesBuilder.Builder as D


notFoundInHTML :: ResponseBuilder
notFoundInHTML =
  notFoundStatus <> html "<h1>404 Not Found</h1>"

notFoundInText :: ResponseBuilder
notFoundInText =
  notFoundStatus <> text "404 Not Found"

badRequestInHTML :: Text -> ResponseBuilder
badRequestInHTML message =
  badRequestStatus <> html ("<h1>Bad Request</h1>" <> "<p>" <> A.text message <> "</p>")

badRequestInText :: Text -> ResponseBuilder
badRequestInText message =
  badRequestStatus <> text (A.text message)

listCredentialsAsJSON :: [(Text, Text)] -> ResponseBuilder
listCredentialsAsJSON =
  jsonBytesBuilder . B.listCredentials

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
listNumbersAsJSON =
  jsonBytesBuilder . B.listNumbers

listNumbersAsHTML :: [Int] -> ResponseBuilder
listNumbersAsHTML numbers =
  html body
  where
    body =
      fromString ("<ul>" <> foldMap (\x -> "<li>" <> show x <> "</li>") numbers <> "</ul>")

jsonBytesBuilder :: D.Literal -> ResponseBuilder
jsonBytesBuilder =
  json . A.bytesBuilder . C.jsonLiteral
