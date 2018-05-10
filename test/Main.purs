module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Kushiyaki (parseURL)
import Test.Assert (assert)
import Type.Prelude (SProxy(..))

main :: Effect Unit
main = do
  -- let (parseURL'
  --       -- inferred type:
  --       :: String -> Either String { name :: String, age :: String })
  let parseURL'
        = parseURL (SProxy :: SProxy "/hello/{name}/{age}")
  let parsed = parseURL' "/hello/Bill/12"
  case parsed of
    Left e -> do
      log $ "didn't work: " <> e
      assert $ 1 == 2
    Right r -> do
      assert $ r.name == "Bill"
      assert $ r.age == "12"

  -- let (parseURL2'
  --       -- inferred type:
  --       :: String -> Either String { name :: String, age :: Int })
  let parseURL2'
        = parseURL (SProxy :: SProxy "/hello/{name:String}/{age:Int}")
  let parsed2 = parseURL2' "/hello/Bill/12"
  case parsed2 of
    Left e -> do
      log $ "didn't work: " <> e
      assert $ 1 == 2
    Right r -> do
      assert $ r.name == "Bill"
      assert $ r.age == 12
