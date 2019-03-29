module Network.HTTP where
import Prelude
import Node.URL as URL
import Data.Options (Option, Options(..), opt, options)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object)

mCHECKOUT :: String
mCHECKOUT    = "CHECKOUT"
mCONNECT :: String
mCONNECT     = "CONNECT"
mCOPY :: String
mCOPY        = "COPY"
mDELETE :: String
mDELETE      = "DELETE"
mGET :: String
mGET         = "GET"
mHEAD :: String
mHEAD        = "HEAD"
mLOCK :: String
mLOCK        = "LOCK"
mM_SEARCH :: String
mM_SEARCH    = "M_SEARCH"
mMERGE :: String
mMERGE       = "MERGE"
mMKACTIVITY :: String
mMKACTIVITY  = "MKACTIVITY"
mMKCOL :: String
mMKCOL       = "MKCOL"
mMOVE :: String
mMOVE        = "MOVE"
mNOTIFY :: String
mNOTIFY      = "NOTIFY"
mOPTIONS :: String
mOPTIONS     = "OPTIONS"
mPATCH :: String
mPATCH       = "PATCH"
mPOST :: String
mPOST        = "POST"
mPROPFIND :: String
mPROPFIND    = "PROPFIND"
mPROPPATCH :: String
mPROPPATCH   = "PROPPATCH"
mPURGE :: String
mPURGE       = "PURGE"
mPUT :: String
mPUT         = "PUT"
mREPORT :: String
mREPORT      = "REPORT"
mSEARCH :: String
mSEARCH      = "SEARCH"
mSUBSCRIBE :: String
mSUBSCRIBE   = "SUBSCRIBE"
mTRACE :: String
mTRACE       = "TRACE"
mUNLOCK :: String
mUNLOCK      = "UNLOCK"
mUNSUBSCRIBE :: String
mUNSUBSCRIBE = "UNSUBSCRIBE"

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

foreign import data Request :: Type

foreign import data Response :: Type

foreign import requestImpl :: Foreign -> (Response -> Effect Unit) -> Effect Request

foreign import setTimeout :: Request -> Int -> Effect Unit -> Effect Unit


request :: Options RequestOptions -> (Response -> Effect Unit) -> Effect Request
request = requestImpl <<< options

requestFromURI :: String -> (Response -> Effect Unit) -> Effect Request
requestFromURI = requestImpl <<< unsafeToForeign <<< URL.parse