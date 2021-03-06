module Strelka.Demo.Route where

import Prelude
import Strelka.RequestParser
import qualified Strelka.Demo.Effect as B
import qualified Strelka.Demo.ResponseBuilder as A
import qualified Strelka.RequestBodyConsumer as E
import qualified Strelka.ResponseBuilder as C
import qualified Data.Attoparsec.ByteString.Char8 as D


type Route =
  RequestParser B.Effect C.ResponseBuilder

top :: Route
top =
  consumeSegmentIfIs "numbers" *> numbers <|>
  consumeSegmentIfIs "credentials" *> credentials <|>
  notFound
  where
    numbers =
      ensureThatMethodIsGet *> get <|>
      ensureThatMethodIsPut *> put <|>
      ensureThatMethodIsDelete *> delete
      where
        get =
          ensureThatAcceptsHTML *> html <|>
          ensureThatAcceptsJSON *> json
          where
            html =
              A.listNumbersAsHTML <$> lift B.listNumbers
            json =
              A.listNumbersAsJSON <$> lift B.listNumbers
        put =
          authorizing "" authorized
          where
            authorized =
              consumingBodyAsInt onInt
              where
                onInt int =
                  lift (B.addNumber int) *> okay
        delete =
          authorizing "" authorized
          where
            authorized =
              consumingBodyAsInt onInt
              where
                onInt int =
                  lift (B.deleteNumber int) *> okay
    credentials =
      output <*> lift B.listCredentials
      where
        output =
          ensureThatAcceptsHTML $> A.listCredentialsAsHTML <|>
          ensureThatAcceptsJSON $> A.listCredentialsAsJSON <|>
          pure A.listCredentialsAsText

-- |
-- Reusable route for wrapping other routes with HTTP Authorization.
authorizing :: ByteString -> Route -> Route
authorizing realm authorized =
  authorize *> (authorized <|> notFound) <|>
  unauthorized
  where
    authorize =
      do
        (username, password) <- getAuthorization
        success <- lift (B.authorize username password)
        guard success
    unauthorized =
      pure (C.unauthorized realm)

okay :: Route
okay =
  pure C.okayStatus

badRequest :: Text -> Route
badRequest message =
  ensureThatAcceptsHTML *> pure (A.badRequestInHTML message) <|>
  pure (A.badRequestInText message)

notFound :: Route
notFound =
  ensureThatAcceptsHTML *> html <|>
  text
  where
    html =
      pure A.notFoundInHTML
    text =
      pure A.notFoundInText

consumingBodyAsInt :: (Int -> Route) -> Route
consumingBodyAsInt onInt =
  do
    parsingResult <- consumeBody (E.bytesParser (D.decimal <* D.endOfInput))
    either badRequest onInt parsingResult
