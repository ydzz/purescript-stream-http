module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Network.HTTP as HTTP
import Node.Stream (end)
main :: Effect Unit
main = do
  log "You should add some tests."
  req <- HTTP.requestFromURI "/index.js" logResp
  end (HTTP.requestAsStream req) (pure unit)
  log "aaa"

logResp::HTTP.Response -> Effect Unit
logResp resp = do
  logShow "resp"
 
