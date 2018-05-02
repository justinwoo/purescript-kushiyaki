module Kushiyaki where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Record as Record
import Data.Record.Builder (Builder)
import Data.Record.Builder as Builder
import Data.Record.Format (class Parse, FCons, FNil, FProxy(..), Lit, Var, kind FList)
import Data.String (Pattern(..), indexOf, splitAt, stripPrefix)
import Global (readInt)
import Prim.Row as Row
import Prim.RowList as RL
import Type.Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Prelude (RLProxy(..))

class ParseURL (url :: Symbol) (row :: # Type) where
  parseURL :: SProxy url -> String -> Either String { | row }

instance parseStringInst ::
  ( Parse url xs
  , ParseURLImpl xs () row
  ) => ParseURL url row where
  parseURL _ s
      = Builder.build <@> {}
    <$> parseURLImpl (FProxy :: FProxy xs) s

class ParseURLImpl (xs :: FList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  parseURLImpl
    :: FProxy xs
    -> String
    -> Either String (Builder { | from } { | to })

instance nilParseURLImpl :: ParseURLImpl FNil () () where
  parseURLImpl _ _ = pure identity

instance consVarParseURLImpl ::
  ( IsSymbol name
  , Row.Cons name String from' to
  , Row.Lacks name from'
  , ParseURLImpl tail from from'
  ) => ParseURLImpl (FCons (Var name) tail) from to where
  parseURLImpl _ s = do
    split' <- split
    let first = Builder.insert nameP split'.before
    rest <- parseURLImpl (FProxy :: FProxy tail) split'.after
    pure $ first <<< rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP
      split = maybe (Left "error") Right $ case indexOf (Pattern "/") s of
        Just idx -> splitAt idx s
        Nothing -> pure { before: s, after: "" }

instance consLitParseURLImpl ::
  ( IsSymbol segment
  , ParseURLImpl tail from to
  ) => ParseURLImpl (FCons (Lit segment) tail) from to where
  parseURLImpl _ s =
    case stripPrefix (Pattern segment) s of
      Nothing ->
        Left $ "could not strip segment " <> segment <> " from path " <> s
      Just remaining ->
        parseURLImpl (FProxy :: FProxy tail) remaining
    where
      segment = reflectSymbol (SProxy :: SProxy segment)

-- convert path param strings
class ReadParam a where
  readParam :: String -> Either String a

instance stringReadParam :: ReadParam String where
  readParam s = pure s

instance intReadParam :: ReadParam Int where
  readParam s =
    case fromNumber $ readInt 10 s of
      Just a -> pure a
      Nothing ->
        Left $ "could not parse " <> s <> " into integer"

class ConvertRecord (i :: # Type) (o :: # Type) where
  convertRecord :: { | i } -> Either String { | o}

instance convertRecordInst ::
  ( RL.RowToList o os
  , ConvertRecordFields os i () o
  ) => ConvertRecord i o where
  convertRecord i
      = Builder.build <@> {}
    <$> convertRecordFields (RLProxy :: RLProxy os) i

class ConvertRecordFields (os :: RL.RowList) (r :: # Type) (i :: # Type) (o :: # Type)
  | os -> r i o
  where
    convertRecordFields
      :: RLProxy os
      -> { | r }
      -> Either String (Builder { | i } { | o })

instance nilConvertRecordFields :: ConvertRecordFields RL.Nil r () () where
  convertRecordFields _ _ = pure identity

instance consConvertRecordFields ::
  ( IsSymbol name
  , ReadParam ty
  , Row.Cons name String r' r
  , Row.Cons name ty from' to
  , Row.Lacks name from'
  , ConvertRecordFields tail r from from'
  ) => ConvertRecordFields (RL.Cons name ty tail) r from to where
  convertRecordFields _ r = do
    value <- readParam str
    let first = Builder.insert nameP value
    rest <- convertRecordFields (RLProxy :: RLProxy tail) r
    pure $ first <<< rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP
      str = Record.get nameP r
