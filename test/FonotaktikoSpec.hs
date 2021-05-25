{-# LANGUAGE BlockArguments #-}
module FonotaktikoSpec where

import Control.Arrow
import Test.Hspec
import Fonotaktiko

spec :: Spec
spec = do
   describe "Fonotaktiko" do
      it "can read valid base words" do
         map ĉuValidaVorto
            ["Marika", "Runa", "Rejna", "Nivo", "Rini", "Silika", "Evra"]
            `shouldSatisfy` and