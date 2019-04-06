module Network.HTTP.Internal where
import Prelude

import Data.Maybe (Maybe)
import Data.Options (Option, Options, opt, options)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object as O
import Node.Stream (Writable, Readable)
import Node.URL as URL
import Unsafe.Coerce (unsafeCoerce)

data RequestFamily = IPV4 | IPV6

data RequestOptions

newtype RequestHeaders = RequestHeaders (O.Object String)

protocol :: Option RequestOptions String
protocol = opt "protocol"

hostname :: Option RequestOptions String
hostname = opt "hostname"

port :: Option RequestOptions Int
port = opt "port"

method :: Option RequestOptions String
method = opt "method"

path :: Option RequestOptions String
path = opt "path"

headers :: Option RequestOptions RequestHeaders
headers = opt "headers"

auth :: Option RequestOptions String
auth = opt "auth"

key :: Option RequestOptions String
key = opt "key"

cert :: Option RequestOptions String
cert = opt "cert"


foreign import data Request :: Type

foreign import data Response :: Type

requestAsStream :: forall r. Request -> Writable r
requestAsStream = unsafeCoerce

responseAsStream :: forall w. Response -> Readable w
responseAsStream = unsafeCoerce


foreign import requestImpl ::Foreign -> (Response-> Effect Unit) -> Effect Request

foreign import setTimeout ::Response -> Int -> Effect Unit -> Effect Unit


request' ::Options RequestOptions -> (Response -> Effect Unit) -> Effect Request
request' = requestImpl <<< options

requestFromURI ::String -> (Response -> Effect Unit) -> Effect Request
requestFromURI = requestImpl <<< unsafeToForeign <<< URL.parse

httpVersion :: Response -> String
httpVersion = _.httpVersion <<< unsafeCoerce

headers' :: forall a. Response -> O.Object a
headers' = _.headers <<< unsafeCoerce


responseHeaders :: Response -> O.Object String
responseHeaders res = O.delete "set-cookie" $ headers' res

responseCookies :: Response -> Maybe (Array String)
responseCookies res = O.lookup "set-cookie" $ headers' res


statusCode :: Response -> Int
statusCode = _.statusCode <<< unsafeCoerce

statusMessage :: Response -> String
statusMessage = _.statusMessage <<< unsafeCoerce