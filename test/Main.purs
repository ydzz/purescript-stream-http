module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Network.HTTP as HTTP
main :: Effect Unit
main = do
  log "You should add some tests."
  _ <- HTTP.requestFromURI "http://www.baidu.com/" logResp
  log "aaa"

logResp::HTTP.Response -> Effect Unit
logResp resp = do
  logShow "resp"
