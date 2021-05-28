{-# LANGUAGE BlockArguments #-}
module MalinflektadoSpec where

import Test.Hspec
import Malinflektado
import Vorttipo

testiBazanVorton :: String -> Vorttipo -> Expectation
testiBazanVorton vorto vorttipo = do
   malinflekti vorto `shouldBe`
      Just (MalinflektitaVorto {ŝtupoj=[], bazaTipo=vorttipo, bazaVorto=vorto})

testiInflekcion inflekcio vorto bazaVorto vorttipo = do
   malinflekti vorto `shouldBe`
      Just (MalinflektitaVorto {ŝtupoj=[inflekcio], bazaTipo=vorttipo, bazaVorto=bazaVorto})

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
   
   describe "Substantivoj" do
      it "povas legi PEsti" do
         testiInflekcion PEsti "vilkaa" "vilka" SubstantivoN
         testiInflekcion PEsti "kunaa" "kuna" SubstantivoNN

      it "povas legi Kvalito" do
         testiInflekcion Kvalito "litelkaare" "litelka" SubstantivoN
         testiInflekcion Kvalito "gremure" "gremi" SubstantivoNN
      
      it "povas legi Sola" do
         testiInflekcion Sola "pospira" "pospi" SubstantivoN
         testiInflekcion Sola "rimara" "rima" SubstantivoNN
      
      it "povas legi AEsti" do
         testiInflekcion AEstiA "tatreteva" "tatrete" SubstantivoN
         testiInflekcion AEstiA "kunava" "kuna" SubstantivoNN