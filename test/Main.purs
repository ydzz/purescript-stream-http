module Test.Main where

import Prelude

import Data.Map as M
import Effect (Effect)
import Effect.Console (log, logShow)
import Foreign.Object as O
import Network.HTTP as HTTP
import Node.Stream (Readable)
import Node.URL as URL
main :: Effect Unit
main = do
  logShow $ URL.parse "http://www.baidu.com:8800?aa=123&b=asd"
  ttt
  log "You should add some tests."

ttt::Effect Unit
ttt = do
  let (reqOpt::HTTP.Request) = { reqURL : URL.parse "/index.js",reqMethod:HTTP.POST,reqHeader:O.empty}
  _ <- HTTP.request reqOpt onResp
  logShow "end"

onResp::forall w. (Readable w) -> Effect Unit
onResp resp = do
  logShow ">>>"