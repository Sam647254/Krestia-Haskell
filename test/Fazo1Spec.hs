{-# LANGUAGE BlockArguments #-}
module Fazo1Spec where

import Test.Hspec
import Sintaksanalizilo
import Sintaksanalizilo.Fazo1
import Testiloj

spec :: Spec
spec = do
   describe "Fazo 1" do
      it "can attach a postfix modifier to a word" do
         let brite = praveMalinflekti "brite"
         let kretega = praveMalinflekti "kretega"
         trakti1 (praveMalinflektiĈiujn "brite kretega") `shouldBe`
            SAFazo1Rezulto
               { modifitajVortoj =
                  [ModifitaVorto { vorto = brite,
                     modifantoj = [MalantaŭModifanto (Atributo (modifitaVorto kretega))] }]
               }
      
      it "can attach a prefix modifier to a word" do
         let grike = praveMalinflekti "grike"
         let kreteva = praveMalinflekti "kreteva"
         trakti1 (praveMalinflektiĈiujn "kreteva grike") `shouldBe`
            SAFazo1Rezulto
               { modifitajVortoj =
                  [ModifitaVorto { vorto = grike,
                     modifantoj = [AntaŭModifanto (Atributo (modifitaVorto kreteva))] }]
               }
      
      it "can attach modifiers to a word" do
         let lumiteva = praveMalinflekti "lumiteva"
         let grike = praveMalinflekti "grike"
         let pritega = praveMalinflekti "pritega"
         trakti1 (praveMalinflektiĈiujn "lumiteva grike pritega") `shouldBe`
            SAFazo1Rezulto
               { modifitajVortoj =
                  [ModifitaVorto { vorto = grike,
                     modifantoj =
                        [ MalantaŭModifanto (Atributo (modifitaVorto pritega))
                        , AntaŭModifanto (Atributo (modifitaVorto lumiteva))
                        ]}]
               }