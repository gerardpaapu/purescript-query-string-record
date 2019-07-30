module Main where

import Prelude

import Data.Int (decimal)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (intercalate)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record (get)
import Type.Data.RowList (RLProxy(..))
import Type.Data.Symbol (reflectSymbol)
import Type.Prelude (class IsSymbol)

main :: Effect Unit
main = do
  log $ toQueryString { "poo%%p": Just 1
                      , farts: true
                      , bum: 3
                      }

type KeyValue
  = { key :: String
    , value :: String
    }
    
class ToParam a where
  toParam :: a -> Maybe String

instance stringToParam :: ToParam String where
  toParam s = Just s

instance intToParam :: ToParam Int where
  toParam = Just <<< Int.toStringAs decimal

instance numberToParam :: ToParam Number where
  toParam = Just <<< show

instance maybeToParam :: ToParam a => ToParam (Maybe a) where
  toParam = (_ >>= toParam)

instance booleanToParam :: ToParam Boolean where
  toParam = if _ then (Just "true") else Nothing

class QueryString a where
  toQueryString :: a -> String

foreign import encodeURIComponent :: String -> String

instance recordToQueryString ::
  ( RowToList fields fieldList
  , WriteQSFields fieldList fields
  ) => QueryString (Record fields) where
  toQueryString rec = "?" <> tail
      where
         pairs = toKeyValuePairs rl rec
         unpair {key, value} =
            encodeURIComponent key <> "=" <> encodeURIComponent value
         tail = intercalate "&" $  unpair <$> pairs
         rl = RLProxy :: RLProxy fieldList


class WriteQSFields (rl :: RowList) row 
  | rl -> row where
  toKeyValuePairs :: forall g. g rl -> Record row -> Array KeyValue

instance consWriteQSFields ::
  ( IsSymbol name
  , Row.Cons name ty whatever row
  , Row.Lacks name whatever
  , ToParam ty
  , WriteQSFields tail row
  ) => WriteQSFields (Cons name ty tail) row where
  toKeyValuePairs _ rec = result
    where
      namep = SProxy :: SProxy name
      value = toParam $ get namep rec
      tailp = RLProxy :: RLProxy tail
      rest = toKeyValuePairs tailp rec
      pair = maybe [] (\v -> [{ key: reflectSymbol namep, value: v }]) value
      result = pair <> rest
      

instance nilWriteQSFields ::
  WriteQSFields Nil row where
  toKeyValuePairs _ _ = []
