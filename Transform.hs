{-# LANGUAGE OverloadedStrings #-}

module Transform where

import Control.Arrow
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as B8L
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as SC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified EDN

ednToJson :: EDN.Value -> JSON.Value
ednToJson EDN.Nil = JSON.Null
ednToJson (EDN.Boolean b) = JSON.Bool b
ednToJson (EDN.String s) = JSON.String s
ednToJson (EDN.Character c) = JSON.String (T.pack [c])
ednToJson (EDN.Symbol sym) = JSON.String sym
ednToJson (EDN.Keyword kw) = JSON.String (T.concat [":", kw])
ednToJson (EDN.WholeNumber i) = JSON.Number (SC.scientific i 0)
ednToJson (EDN.RationalNumber r) = JSON.Number (SC.fromFloatDigits f) where f = fromRational r :: Double
ednToJson (EDN.RealNumber f) = JSON.Number (SC.fromFloatDigits f)
ednToJson (EDN.List vs) = jsonArray vs
ednToJson (EDN.Vector vs) = jsonArray vs
ednToJson (EDN.Set vs) = jsonArray vs
ednToJson (EDN.Map vs) = JSON.Object (HM.fromList $ map (stringify *** ednToJson) vs)
  where
    stringify :: EDN.Value -> T.Text
    stringify (EDN.String s) = s
    stringify (EDN.Keyword kw) = kw -- minus the :
    stringify (EDN.Symbol sym) = sym
    stringify v = (TE.decodeUtf8 . toStrict . JSON.encode . ednToJson) v
    toStrict :: B8L.ByteString -> B8.ByteString
    toStrict = B8.concat . B8L.toChunks

jsonArray :: [EDN.Value] -> JSON.Value
jsonArray = JSON.Array . V.fromList . map ednToJson
