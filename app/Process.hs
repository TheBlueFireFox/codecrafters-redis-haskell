{-# LANGUAGE OverloadedStrings #-}

module Process (process) where

import Data.Text.Lazy qualified as TE
import Request qualified as Req
import Response qualified as Res

handleCommands :: Req.Request -> Either TE.Text Res.Response
handleCommands Req.Request{Req.command, Req.payload} = Right $ case command of
    Req.PING -> Res.Response{Res.command = Res.PONG, Res.payload = payload}

endOfLine :: TE.Text
endOfLine = "\r\n"

process :: TE.Text -> Either TE.Text TE.Text
process input =  Res.build <$> (handleCommands =<< Req.parse input)
