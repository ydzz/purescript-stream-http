module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Network.HTTP as HTTP
import Node.Encoding (Encoding(..))
import Node.Stream (Readable, onDataString)

main :: Effect Unit
main = do
  launchAff_ $ do 
    str <- HTTP.fetchFullString "http://www.baidu.com"
    liftEffect $ logShow str

onResp::forall w. (Readable w) -> Effect Unit
onResp resp = do
  onDataString resp UTF8 (logShow)
  logShow ">>>"