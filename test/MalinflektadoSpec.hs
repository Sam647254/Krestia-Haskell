{-# LANGUAGE BlockArguments #-}
module MalinflektadoSpec where

import Test.Hspec
import Malinflektado
import Vorttipo

testiBazanVorton :: String -> Vorttipo -> Expectation
testiBazanVorton vorto vorttipo = do
   malinflekti vorto `shouldBe`
      Just (MalinflektitaVorto {ŝtupoj=[], bazaTipo=vorttipo, bazaVorto=vorto})

spec :: Spec
spec = do
   describe "Bazaj vortoj" do
      it "povas malinflekti SubstantivoN" do
         testiBazanVorton "vilka" SubstantivoN

      it "povas malinflekti Verbo12" do
         testiBazanVorton "set" Verbo12
         testiBazanVorton "telit" Verbo12
         
      it "povas malinflekti Verbo123" do
         testiBazanVorton "bep" Verbo123          
         testiBazanVorton "pelip" Verbo123