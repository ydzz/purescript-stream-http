module Network.HTTP.QueryString.Stringify(
  stringify,
  defaultStringifyOption,
  StringifyOption(..),
  Encoder(..),
  Filter(..),
  ArrayPrefixGenerator(..),
  QSFormatter(..),
  SortFunc,
  defaultEncode
) where

import Prelude

import Data.Argonaut (Json, caseJson, caseJsonArray, fromObject, fromString, isArray, isBoolean, isNumber, isString, toObject)
import Data.Array (concat, singleton, sortBy)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Foreign.Object (Object, keys)
import Partial.Unsafe (unsafePartial)

newtype　Encoder = Encoder (String -> Encoder -> String -> String)

foreign import defaultEncode ::String -> Encoder -> String -> String

foreign import rFC1738 ::String -> String

--joinJsonArray ["1","2","3"] "." = "1,2,3"
--joinJsonArray ["1","2","3",["4","5"]] "." = "1,2,3,4,5"
foreign import joinJsonArray ::Array Json -> Json -> Json

foreign import jslog ::forall a. a ->  Unit

foreign import jsKeys ::forall a. a ->  Array String

foreign import getObjectByKey ::Json -> String -> Json

data Filter =  FilterFunc (String -> Json -> Json) | FilterArray (Array String)

type SortFunc = Maybe (String -> String -> Ordering)

data ArrayPrefixGenerator = Brackets | Comma | Indices | Repeat

data QSFormatter = RFC1738 | RFC3986

getQSFormatter::QSFormatter ->　(String -> String)
getQSFormatter RFC3986 = identity
getQSFormatter RFC1738 = rFC1738

getArrayPrefix::ArrayPrefixGenerator -> (String -> String -> String)
getArrayPrefix Brackets = \prefix _ -> prefix <> "[]"
getArrayPrefix Comma    = const
getArrayPrefix Indices  = \prefix key -> prefix <> "[" <> key <> "]"
getArrayPrefix Repeat   = \prefix _ -> prefix

type StringifyOption = {
  prefix             ::String,
  generateArrayPrefix::ArrayPrefixGenerator,
  encoder            ::Maybe Encoder,
  filter             ::Maybe Filter,
  allowDots          ::Boolean,
  formatter          ::QSFormatter,
  encodeValuesOnly   ::Boolean,
  charset            ::String,
  sort               ::SortFunc,
  delimiter          ::String,
  addQueryPrefix     ::Boolean
}

defaultStringifyOption::StringifyOption
defaultStringifyOption = {
   prefix:"",
   generateArrayPrefix:Indices,
   encoder:Just $ Encoder defaultEncode,
   filter:Nothing,
   allowDots:false,
   formatter:RFC3986,
   encodeValuesOnly:false,
   charset:"utf8",
   sort:Nothing,
   delimiter:"&",
   addQueryPrefix:true
 }

stringify::Object Json -> StringifyOption -> String
stringify json param = let arr       = concat $ map mapFunc (sortedKeys objKeys)
                           joinedStr = showJson $ joinJsonArray arr (fromString param.delimiter)
                       in if param.addQueryPrefix then "?" <> joinedStr else joinedStr
 where
  sortedKeys::Array String -> Array String
  sortedKeys keys = maybe keys (\s-> sortBy s keys) param.sort 
  objKeys::Array String
  objKeys = maybe (keys json) filterFunc param.filter
  filterFunc (FilterFunc fn) = keys $ unsafePartial $ fromJust $ toObject $ fn "" (fromObject json)
  filterFunc (FilterArray arr) = arr
  mapFunc::String -> Array Json
  mapFunc  k = runIdentity $ _stringify (getObjectByKey (fromObject json) k) k param.generateArrayPrefix param.encoder 
                                        param.filter param.sort param.allowDots param.formatter param.encodeValuesOnly param.charset


_stringify::Json -> String -> ArrayPrefixGenerator -> Maybe Encoder ->
            Maybe Filter -> SortFunc -> Boolean -> QSFormatter -> Boolean -> String -> Identity (Array Json)
_stringify object prefix generateArrayPrefix encoder filter sortFunc  allowDots formatter encodeValuesOnly charset = do
   let obj = case filter of
                        Just (FilterFunc  ffn)  -> ffn prefix object
                        Just (FilterArray arr) -> case generateArrayPrefix of
                                                   Comma     -> caseJsonArray object joinedArray object
                                                   otherwise -> object
                        Nothing -> object
   pure $ ifJsonType obj
 where
   joinedArray::(Array Json) -> Json
   joinedArray jarr = joinJsonArray jarr (fromString ".")
   ifJsonType::Json -> Array Json
   ifJsonType json | (isString json || isBoolean json || isNumber json) = singleton $ maybe (returnNoEncoder json) (returnEncoder json) encoder
   ifJsonType json |  otherwise = let sortedKeys = getKeys json filter in  concat $ map (mapFunc json) sortedKeys
   returnNoEncoder::Json -> Json
   returnNoEncoder   obj = fromString $ (formatterFunc prefix) <> "=" <> formatterFunc (showJson obj)
   returnEncoder::Json -> Encoder -> Json
   returnEncoder obj e@(Encoder encoderfn) = let  keyValue = if encodeValuesOnly then prefix else (encoderfn prefix e charset)
                                             in   fromString $ (formatterFunc keyValue) <> "=" <> (formatterFunc $ encoderfn (showJson obj) e charset)
   formatterFunc::(String->String)
   formatterFunc = getQSFormatter formatter
   getKeys::Json -> Maybe Filter -> Array String
   getKeys json (Just (FilterArray arr)) = arr
   getKeys json (Just _)                =  []
   getKeys json Nothing  = let okeys = jsKeys json
                           in  maybe okeys (\s->sortBy s okeys) sortFunc
   mapFunc::Json -> String -> Array Json
   mapFunc json k = if isArray json 
                    then  runIdentity $  _stringify (getObjectByKey json k) (arrayPrefix k) generateArrayPrefix encoder filter 
                                                   sortFunc allowDots formatter encodeValuesOnly charset
                    else  runIdentity $  _stringify (getObjectByKey json k) (if allowDots then "." <> k else "["<>k<>"]") generateArrayPrefix 
                                                    encoder filter sortFunc allowDots formatter encodeValuesOnly charset
   arrayPrefix::String -> String
   arrayPrefix k = case generateArrayPrefix of
         Comma      -> prefix
         ap         -> (getArrayPrefix ap) prefix k  

showJson::Json -> String
showJson json =  caseJson  (const "()")  -- Unit -> a 
                                        show                   -- Boolean -> a
                                        show                   -- Number -> a
                                        identity               -- String -> a
                                        (const "[Array]")      -- (Array Json -> a)
                                        (const "[Object]")     --(Object Json -> a)
                                        json

runIdentity::forall a.Identity a -> a
runIdentity (Identity a) = a

