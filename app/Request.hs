{-# LANGUAGE OverloadedStrings #-}

module Request (Request (..), Command (..), parse) where

import Data.Text.Internal.Lazy.Search qualified as TES
import Data.Text.Lazy qualified as TE

data Command
    = PING
    deriving (Show)

data Request = Request
    { command :: Command
    , payload :: Maybe TE.Text
    }
    deriving (Show)

parsePing :: TE.Text -> Request
parsePing _ = Request{command = PING, payload = Nothing}

parse :: TE.Text -> Either TE.Text Request
parse str =
    if (not . null) (TES.indices "ping" (TE.toLower str))
        then Right $ parsePing str
        else Left $ "unsupported : " <> str

-- case head w of
--     "PING" -> Right $ parsePing str
--     com -> Left $ "unsupported : " <> com
