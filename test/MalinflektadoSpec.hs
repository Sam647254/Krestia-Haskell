{-# LANGUAGE BlockArguments #-}
module MalinflektadoSpec where

import Test.Hspec
import Malinflektado
import Vorttipo

spec :: Spec
spec = do
   describe "Bazaj vortoj" do
      it "povas malinflekti Verbo12" do
         malinflekti "set" `shouldBe`
            Just (MalinflektitaVorto {ŝtupoj=[], bazaTipo=Verbo12, bazaVorto="set"})
         
         malinflekti "telit" `shouldBe`
            Just (MalinflektitaVorto {ŝtupoj=[], bazaTipo=Verbo12, bazaVorto="telit"})