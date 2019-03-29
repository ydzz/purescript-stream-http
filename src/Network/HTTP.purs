module Network.HTTP where
import Prelude

import Data.Options (Option, Options, opt, options)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object)
import Node.Stream (Readable, Writable)
import Node.URL as URL
import Unsafe.Coerce (unsafeCoerce)

data RequestFamily = IPV4 | IPV6

data RequestOptions

newtype RequestHeaders = RequestHeaders (Object String)

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



foreign import requestImpl :: Foreign -> (Response -> Effect Unit) -> Effect Request

foreign import setTimeout :: Request -> Int -> Effect Unit -> Effect Unit


request :: Options RequestOptions -> (Response -> Effect Unit) -> Effect Request
request = requestImpl <<< options


foreign import data Request :: Type

foreign import data Response :: Type

responseAsStream :: forall w. Response -> Readable w
responseAsStream = unsafeCoerce

requestAsStream :: forall r. Request -> Writable r
requestAsStream = unsafeCoerce

requestFromURI :: String -> (Response -> Effect Unit) -> Effect Request
requestFromURI = requestImpl <<< unsafeToForeign <<< URL.parse