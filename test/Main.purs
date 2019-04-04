module Test.Main where

import Prelude

import Data.Argonaut (fromArray, fromObject, fromString)
import Data.Array as A
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (logShow)
import Foreign.Object as O
import Network.HTTP as HTTP
import Network.HTTP.QueryString as QS
import Node.Stream (Readable)
import Node.URL as URL
main :: Effect Unit
main = do
  logShow $ URL.parse "http://www.baidu.com:8800?aa=123&b=asd"
  testQueryString
  logShow "end"
ttt::Effect Unit
ttt = do
  let (reqOpt::HTTP.Request) = { reqURL : URL.parse "/index.js",reqMethod:HTTP.POST,reqHeader:O.empty}
  _ <- HTTP.request reqOpt onResp
  logShow "end"

onResp::forall w. (Readable w) -> Effect Unit
onResp resp = do
  logShow ">>>"

testQueryString::Effect Unit
testQueryString = do
 let s = QS.stringify (O.fromFoldable ["fucker" /\ fromString "kkk","cc" /\ fromArray [fromString "123"] ]) QS.defaultStringifyOption
 logShow s
 logShow "testQueryString end"

