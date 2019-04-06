module Network.HTTP (
  fetchFullBuffer,
  fetchFullString,
  request,
  RequestMethod(..),
  RequestOption(..),
  module Internal
) where

import Prelude

import Data.Either (Either(..))
import Data.Options (options, (:=))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Error, makeAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Foreign.Object as O
import Network.HTTP.Internal as Internal
import Node.Buffer as Buf
import Node.Encoding (Encoding(..))
import Node.Stream (Readable, Writable, end, onData, onEnd, onError)
import Node.URL as URL
import Unsafe.Coerce (unsafeCoerce)

data RequestMethod = HEAD  | PUT | GET | POST | DELETE | OPTIONS |
                     TRACE | CONNECT | Custom String

derive instance eqRequestMethod :: Eq RequestMethod
instance showRequestMethod :: Show RequestMethod where
   show HEAD       = "HEAD"
   show PUT        = "PUT"
   show GET        = "GET"
   show POST       = "POST"
   show DELETE     = "DELETE"
   show OPTIONS    = "OPTIONS"
   show TRACE      = "TRACE"
   show CONNECT    = "CONNECT"
   show (Custom s) = s

type RequestOption =  {
  reqURL    ::URL.URL,
  reqMethod ::RequestMethod,
  reqHeader ::O.Object String
}

request::RequestOption -> (Internal.Response -> Effect Unit) -> Effect Internal.Request
request req callback = do
  let obj = reqToObject req
  Internal.requestImpl obj callback

reqToObject::RequestOption -> Foreign
reqToObject req = options $ 
                  Internal.hostname := (unsafeCoerce req.reqURL.hostname) <>
                  Internal.protocol := (unsafeCoerce req.reqURL.protocol) <>
                  Internal.port     := (unsafeCoerce req.reqURL.port) <>
                  Internal.path     := (unsafeCoerce req.reqURL.path) <>
                  Internal.method   := show req.reqMethod <>
                  Internal.headers  := Internal.RequestHeaders req.reqHeader


fetchFullString::String -> Aff (Either Error String)
fetchFullString url = do
  buff <- fetchFullBuffer url
  case buff of
    Left err -> pure $ Left err 
    Right r  -> do 
      strBuf <- liftEffect $ (Buf.toString UTF8 r)
      pure $ Right strBuf

fetchFullBuffer::String -> Aff (Either Error Buf.Buffer)
fetchFullBuffer url = do
  makeAff runRequest
 where
  reqOptions = {reqURL : URL.parse url,reqMethod:GET,reqHeader:O.fromFoldable ["Access-Control-Allow-Origin" /\ "*"]}
  runRequest::(Either Error (Either Error Buf.Buffer) -> Effect Unit) -> Effect Canceler
  runRequest ef = do
    req <- request reqOptions (onReadFunc ef)
    let reqWriter = Internal.requestAsStream req
    end reqWriter (pure unit)
    onError reqWriter \err -> do
        ef $ Right $ Left err
    pure $ Canceler (\e -> pure unit)
  onReadFunc::(Either Error (Either Error Buf.Buffer) -> Effect Unit) -> Internal.Response -> Effect Unit
  onReadFunc ef read = do
   emptyBuff <- Buf.create 0
   recvBuff <- Ref.new emptyBuff
   let readw = Internal.responseAsStream read
   onData readw  (\inBuffer -> do
     Ref.modify_ (\srcBuf -> unsafePerformEffect $ Buf.concat [srcBuf,inBuffer]) recvBuff
   )
   onEnd readw $ do
    allBuff <- Ref.read recvBuff
    ef $ Right $ Right allBuff
   --onError read \err -> do
   --     logShow "errrrrrrrrrrrrrrrrrrrrrr"
   --     ef $ Right $ Left err
   pure unit