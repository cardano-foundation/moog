module Lib.JSON.Canonical.Aeson (fromAeson, fromCanon, fromScientific) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromJust)
import Data.Scientific
    ( Scientific
    , scientific
    , toBoundedInteger
    )
import Data.Text (pack, unpack)
import Data.Vector qualified as V

import Text.JSON.Canonical qualified as C

fromCanon :: C.JSValue -> A.Value
fromCanon (C.JSBool b) = A.Bool b
fromCanon C.JSNull = A.Null
fromCanon (C.JSNum n) = A.Number (scientific (fromIntegral n) 0)
fromCanon (C.JSString s) = A.String $ pack $ C.fromJSString s
fromCanon (C.JSArray xs) = A.Array $ V.fromList $ fmap fromCanon xs
fromCanon (C.JSObject xs) = A.Object $ KM.fromList $ map fromCanonKV xs

fromAeson :: A.Value -> Maybe C.JSValue
fromAeson (A.Bool b) = Just (C.JSBool b)
fromAeson A.Null = Just C.JSNull
fromAeson (A.Number x) = fromScientific x
fromAeson (A.Array xs) = C.JSArray <$> traverse fromAeson (V.toList xs)
fromAeson (A.String t) = Just $ C.JSString $ C.toJSString $ unpack t
fromAeson (A.Object xs) = Just $ C.JSObject $ map fromAesonKV $ KM.toList xs

fromScientific :: Scientific -> Maybe C.JSValue
fromScientific = fmap C.JSNum . toBoundedInteger

fromCanonKV :: (C.JSString, C.JSValue) -> (K.Key, A.Value)
fromCanonKV (k, v) = (K.fromText $ pack $ C.fromJSString k, fromCanon v)

fromAesonKV :: (K.Key, A.Value) -> (C.JSString, C.JSValue)
fromAesonKV (k, v) = (C.toJSString $ unpack $ K.toText k, fromJust $ fromAeson v)
