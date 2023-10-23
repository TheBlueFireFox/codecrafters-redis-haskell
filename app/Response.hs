{-# LANGUAGE OverloadedStrings #-}

module Response (Response (..), Command (..), build) where

import Data.Text.Lazy qualified as TE

import Request qualified as Req

data Response = Response
    { command :: Command
    , payload :: Maybe TE.Text
    }
    deriving (Show)

data Command = PONG
    deriving (Show)

handlePong Nothing = "PONG"
handlePong (Just p) = p

commandToStr PONG = handlePong

endOfLine = "\r\n"

build Response{command, payload} = "+" <> commandToStr command payload <> endOfLine
