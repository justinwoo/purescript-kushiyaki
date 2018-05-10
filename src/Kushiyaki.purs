module Kushiyaki where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), indexOf, splitAt, stripPrefix)
import Global (readInt)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.Format (class Parse, FCons, FNil, FProxy(..), Lit, Var, kind FList)
import Type.Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

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
  ( ParseTypedParam s name ty
  , ReadParam ty
  , IsSymbol name
  , Row.Cons name ty from' to
  , Row.Lacks name from'
  , ParseURLImpl tail from from'
  ) => ParseURLImpl (FCons (Var s) tail) from to where
  parseURLImpl _ s = do
    split' <- split
    value <- readParam split'.before
    let first = Builder.insert nameP value
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

-- parse out the name and type from a symbol
-- e.g. "name:Int" "name" Int
-- defaults to String
class ParseTypedParam (s :: Symbol) (name :: Symbol) (ty :: Type) | s -> name ty
instance parseTypedParam ::
  ( Symbol.Cons x xs s
  , ParseTypedParamImpl x xs "" name ty
  ) => ParseTypedParam s name ty

class ParseTypedParamImpl
  (x :: Symbol) (xs :: Symbol) (acc :: Symbol)
  (name :: Symbol) (ty :: Type)
  | x xs acc -> name ty
instance noMatchTypedParamImpl ::
  ( Symbol.Append acc x name
  ) => ParseTypedParamImpl x "" acc name String
else instance colonSplitParseTypedParamImpl ::
  ( MatchTypeName tyName ty
  ) => ParseTypedParamImpl ":" tyName name name ty
else instance baseParseTypedParamImpl ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x acc'
  , ParseTypedParamImpl y ys acc' name ty
  ) => ParseTypedParamImpl x xs acc name ty

class MatchTypeName (s :: Symbol) (ty :: Type) | s -> ty
instance stringParamTypeSymbol :: MatchTypeName "String" String
else instance intParamTypeSymbol :: MatchTypeName "Int" Int

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
