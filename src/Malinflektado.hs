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
import Control.Arrow

newtype Legilo a = Legilo (String -> Maybe (a, String))

data MalinflektaŜtupo
   = NebazaŜtupo (Inflekcio, Vorttipo)
   | BazaŜtupo Vorttipo
   deriving Show

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

legiFinaĵon :: Legilo MalinflektaŜtupo
legiFinaĵon = Legilo f where
   f :: String -> Maybe (MalinflektaŜtupo, String)
   f [] = Nothing
   f vorto = (find (\(finaĵo, _, _) -> finaĵo `isSuffixOf` vorto) finaĵojKajĴustajSekvaĵoj
      & fmap (\(f, i, vt) -> (NebazaŜtupo (i, vt), take (length vorto - length f) vorto)))
      <|> apliki legiBazanVorton vorto

legiBazanVorton :: Legilo MalinflektaŜtupo
legiBazanVorton = Legilo f where
   f [] = Nothing
   f vorto = bazaFinaĵoDe vorto & fmap (\v -> (BazaŜtupo v, vorto))

legiLastanŜtupon :: Legilo [MalinflektaŜtupo]
legiLastanŜtupon = do
   b <- legiBazanVorton
   return [b]

tuteMalinflekti :: String -> Vorttipo -> Maybe ([MalinflektaŜtupo], String)
tuteMalinflekti vorto pravaVorttipo = do
   (sekvaŜtupo, restanta) <- apliki legiFinaĵon vorto
   case sekvaŜtupo of
      NebazaŜtupo (_, s) -> do
         (restantaj, lasta) <- tuteMalinflekti restanta s <|> apliki legiLastanŜtupon restanta
         case restantaj of
            (NebazaŜtupo (_, vt) : _) ->
               if vt == s then
                  return (sekvaŜtupo : restantaj, lasta)
               else do
                  (baza, lasta) <- apliki legiBazanVorton vorto
                  return ([baza], lasta)
            (BazaŜtupo vt : _) ->
               if vt == s then
                  return (sekvaŜtupo : restantaj, lasta)
               else do
                  (baza, lasta) <- apliki legiBazanVorton vorto
                  return ([baza], lasta)
            _ -> undefined
      BazaŜtupo v -> do
         guard (v == pravaVorttipo || pravaVorttipo == Ĉio)
         return ([sekvaŜtupo], restanta)

malinflekti :: String -> Maybe ([MalinflektaŜtupo], String)
malinflekti vorto = tuteMalinflekti vorto Ĉio

kontroliŜtupojn :: [MalinflektaŜtupo] -> Maybe [MalinflektaŜtupo]
kontroliŜtupojn ŝtupoj =
   case last ŝtupoj of
      BazaŜtupo _ -> Just ŝtupoj
      _ -> Nothing