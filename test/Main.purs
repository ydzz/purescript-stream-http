module Test.Main where

import Prelude

import Data.Argonaut (fromArray, fromString)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (logShow)
import Foreign.Object as O
import Network.HTTP.QueryString (ArrayPrefixGenerator(..))
import Network.HTTP.QueryString as QS
import Node.Stream (Readable)
import Node.URL as URL
main :: Effect Unit
main = do
  logShow $ URL.parse "http://www.baidu.com:8800?aa=123&b=asd"
  testQueryString
  logShow "end"


onResp::forall w. (Readable w) -> Effect Unit
onResp resp = do
  logShow ">>>"

testQueryString::Effect Unit
testQueryString = do
 let s = QS.stringify (O.fromFoldable ["fucker" /\ fromString "kkk","cc" /\ fromArray [fromString "123",fromString "456"] ]) 
         (QS.defaultStringifyOption {generateArrayPrefix = Repeat})
 logShow s
 logShow "testQueryString end"

