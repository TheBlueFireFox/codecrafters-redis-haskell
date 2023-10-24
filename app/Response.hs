{-# LANGUAGE OverloadedStrings #-}

module Response (Response (..), Command (..), build) where

import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text.Lazy qualified as TE

data Response = Response
    { command :: Command
    , payload :: Maybe TE.Text
    }
    deriving (Show)

data Command
    = PONG
    | ERROR
    deriving (Show)

handleError :: (IsString a, Semigroup a) => Maybe a -> a
handleError err = "ERR: " <> fromMaybe "something went wrong on the server" err

handlePong :: (IsString a) => Maybe a -> a
handlePong = fromMaybe "PONG"

commandToStr :: (Semigroup a, IsString a) => Command -> Maybe a -> a
commandToStr ERROR payload = "-" <> handleError payload
commandToStr PONG payload = "+" <> handlePong payload

endOfLine :: TE.Text
endOfLine = "\r\n"

build :: Response -> TE.Text
build Response{command, payload} = commandToStr command payload <> endOfLine
