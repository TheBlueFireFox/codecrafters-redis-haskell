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

endOfLine :: String
endOfLine = "\r\n"

-- * 1\r\n$4\r\nping\r\n

parse :: TE.Text -> Either TE.Text Request
parse x =
    if (not . null) (TES.indices "ping" (TE.toLower x))
        then Right $ parsePing x
        else Left $ "unsupported command <" <> TE.replace (TE.pack "\r\n") (TE.pack "\\r\\n") x <> ">"

-- case head w of
--     "PING" -> Right $ parsePing str
--     com -> Left $ "unsupported : " <> com
