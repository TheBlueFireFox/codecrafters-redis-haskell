module Process (process) where

import Data.Text.Lazy qualified as TE
import Request qualified as Req
import Response qualified as Res

handleCommands :: Req.Request -> Res.Response
handleCommands Req.Request{Req.command, Req.payload} = case command of
    Req.PING -> Res.Response{Res.command = Res.PONG, Res.payload = payload}

process :: TE.Text -> Either TE.Text TE.Text
process input = do
    req <- Req.parse input
    let res = handleCommands req
    pure $ Res.build res
