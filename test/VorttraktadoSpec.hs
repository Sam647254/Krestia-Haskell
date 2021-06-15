{-# LANGUAGE BlockArguments #-}
module VorttraktadoSpec where

import Test.Hspec
import Malinflektado
import Vorttraktado

testiValencon :: String -> Int -> Expectation
testiValencon vorto pravaValenco = do
   case malinflekti vorto of
      Right malinflektitaVorto ->
         valencoDe malinflektitaVorto `shouldBe` pravaValenco
      Left eraro -> fail eraro

spec :: Spec
spec = do
   describe "Bazaj vortoj" do
      it "povas kalkuli la valencon" do
         testiValencon "set" 2
         testiValencon "glap" 3