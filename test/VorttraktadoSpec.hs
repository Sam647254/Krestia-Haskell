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

testiĈuPredikato :: String -> Expectation
testiĈuPredikato vorto = do
   case malinflekti vorto of
      Right v -> v `shouldSatisfy` ĉuPredikato
      Left e -> fail e 

spec :: Spec
spec = do
   describe "Bazaj vortoj" do
      it "povas kalkuli la valencon" do
         testiValencon "set" 2
         testiValencon "glap" 3
      
      it "poval kalkuli ĉu predikato" do
         testiĈuPredikato "set"
         testiĈuPredikato "kunalas"
         testiĈuPredikato "kunalasea"
         testiĈuPredikato "betro"