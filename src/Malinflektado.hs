{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
module Malinflektado where

-- many legiFinaĵon <|> legiBazanVorton

import Control.Monad
import Data.List
import Vorttipo
import Data.Function
import Data.Maybe
import Control.Applicative

newtype Legilo a = Legilo (String -> Maybe (a, String))

apliki :: Legilo a -> String -> Maybe (a, String)
apliki (Legilo legilo) = legilo

instance Functor Legilo where
   fmap = liftM

instance Applicative Legilo where
   pure valuo = Legilo (\eniro -> Just (valuo, eniro))
   (<*>) = ap

instance Alternative Legilo where
   empty = Legilo (const Nothing)
   (<|>) legilo1 legilo2 = Legilo f where
      f eniro =
         let rezulto1 = apliki legilo1 eniro in
            if isNothing rezulto1 then
               apliki legilo2 eniro
            else
               rezulto1

instance Monad Legilo where
   return = pure

   (>>=) :: Legilo a -> (a -> Legilo b) -> Legilo b
   (>>=) legilo legiloF = Legilo (\eniro -> do
      (valuo, restanta) <- apliki legilo eniro
      (valuo2, restanta2) <- apliki (legiloF valuo) restanta
      return (valuo2, restanta2))

legiFinaĵon :: Legilo (Inflekcio, Vorttipo)
legiFinaĵon = Legilo f where
   f :: String -> Maybe ((Inflekcio, Vorttipo), String)
   f [] = Nothing
   f vorto =
      let
         rezulto = find (\(finaĵo, _, _) -> finaĵo `isSuffixOf` vorto) finaĵojKajĴustajSekvaĵoj
      in
      fmap (\(f, i, vt) -> ((i, vt), take (length vorto - length f) vorto)) rezulto