module Network.HTTP.Internal where
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



foreign import requestImpl ::forall w. Foreign -> ((Readable w) -> Effect Unit) -> Effect (Writable w)

foreign import setTimeout ::forall w. (Writable w) -> Int -> Effect Unit -> Effect Unit


request ::forall w. Options RequestOptions -> ((Readable w) -> Effect Unit) -> Effect (Writable w)
request = requestImpl <<< options


requestFromURI ::forall w. String -> ((Readable w) -> Effect Unit) -> Effect (Writable w)
requestFromURI = requestImpl <<< unsafeToForeign <<< URL.parse