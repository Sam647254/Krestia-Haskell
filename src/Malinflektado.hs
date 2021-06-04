{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Malinflektado where

-- many legiFinaĵon <|> legiBazanVorton

import Control.Monad
import Data.List
import qualified Data.Text as T
import Vorttipo
import Data.Function
import Data.Maybe
import Control.Applicative
import Control.Arrow
import Fonotaktiko

newtype Legilo a = Legilo (String -> Maybe (a, String))

data MalinflektaŜtupo
   = NebazaŜtupo (Inflekcio, [Vorttipo])
   | BazaŜtupo Vorttipo
   deriving Show

data MalinflektitaVorto = MalinflektitaVorto
   { ŝtupoj :: [Inflekcio]
   , bazaTipo :: Vorttipo
   , bazaVorto :: String
   }
   deriving (Show, Eq)

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

proviTuteMalinflekti :: (Finaĵo, Inflekcio, [Vorttipo]) -> [Vorttipo] -> String -> Maybe ([MalinflektaŜtupo], String)
proviTuteMalinflekti (finaĵo, inflekcio, vorttipoj) pravajVorttipoj vorto =
   let
      pravaInflekcio =
         concatMap ((\p -> map (\pvt -> p `semblas` pvt) pravajVorttipoj) . (inflekcio,)) vorttipoj & or
   in
   if finaĵo `isSuffixOf` vorto && (pravajVorttipoj == [Ĉio] || pravaInflekcio) then do
      let restanta = take (length vorto - length finaĵo) vorto
      case uzasPEsti inflekcio of
         Just pi -> do
            (ŝtupoj, bazaVorto) <- tuteMalinflekti2 vorttipoj restanta
            case ŝtupoj of
               (NebazaŜtupo (PEsti, _) : restantajŜtupoj) ->
                  return (NebazaŜtupo (pi, vorttipoj) : restantajŜtupoj, bazaVorto)
               restantajŜtupoj -> do
                  return (NebazaŜtupo (inflekcio, vorttipoj) : restantajŜtupoj, bazaVorto)
         Nothing ->
            if inflekcio == Kvalito then do
               (ŝtupoj, bazaVorto) <- tuteMalinflekti2 vorttipoj restanta
               case ŝtupoj of
                  (NebazaŜtupo (PEsti, _) : restantajŜtupoj) ->
                     return (NebazaŜtupo (Kvalito , vorttipoj) : restantajŜtupoj, bazaVorto)
                  restantajŜtupoj -> Nothing
            else do
               (restantajŜtupoj, bazaVorto) <- tuteMalinflekti2 vorttipoj restanta
               return (NebazaŜtupo (inflekcio, vorttipoj) : restantajŜtupoj, bazaVorto)
   else
      Nothing

tuteMalinflekti2Ak :: [(Finaĵo, Inflekcio, [Vorttipo])] -> [Vorttipo]
   -> [MalinflektaŜtupo] -> String -> Maybe ([MalinflektaŜtupo], String)
tuteMalinflekti2Ak [] pravajVorttipoj ŝtupoj vorto = (do
   (pEstiŜtupo, restantaVorto) <- apliki legiPEsti vorto
   (restantajŜtupoj, bazaVorto) <- tuteMalinflekti2 pravajVorttipoj restantaVorto
   return (pEstiŜtupo : restantajŜtupoj <> ŝtupoj, bazaVorto))
   <|> (do
      (antaŭigitaŜtupo, restantaVorto) <- apliki legiAntaŭigita vorto
      (restantajŜtupoj, bazaVorto) <- tuteMalinflekti2 pravajVorttipoj restantaVorto
      return (antaŭigitaŜtupo : restantajŜtupoj, bazaVorto))
   <|> (do
   (lastaŜtupo, bazaVorto) <- apliki legiBazanVorton vorto
   case lastaŜtupo of
      BazaŜtupo vt | vt `elem` pravajVorttipoj || pravajVorttipoj == [Ĉio] ->
         return ([lastaŜtupo], bazaVorto)
      _ -> Nothing)

tuteMalinflekti2Ak (sekva : restantaj) pravajVorttipoj ŝtupoj vorto =
   case proviTuteMalinflekti sekva pravajVorttipoj vorto of
      Just (restantajŜtupoj, bazaVorto) ->
         Just (restantajŜtupoj <> ŝtupoj, bazaVorto)
      Nothing -> tuteMalinflekti2Ak restantaj pravajVorttipoj ŝtupoj vorto

tuteMalinflekti2 :: [Vorttipo] -> String -> Maybe ([MalinflektaŜtupo], String)
tuteMalinflekti2 pravajVorttipoj = tuteMalinflekti2Ak finaĵojKajĴustajSekvaĵoj pravajVorttipoj []

malinflekti2 :: String -> Maybe ([MalinflektaŜtupo], String)
malinflekti2 = tuteMalinflekti2 [Ĉio]

legiPEsti :: Legilo MalinflektaŜtupo
legiPEsti = Legilo f where
   f [] = Nothing
   f vorto = do
      (vorttipo, _) <-
         find (\(tipo, finaĵoj) -> any (`isSuffixOf` vorto) finaĵoj) finaĵojDePEsti
      return (NebazaŜtupo (PEsti, [vorttipo]), pAlD vorto)
   
legiAntaŭigita :: Legilo MalinflektaŜtupo
legiAntaŭigita = Legilo f where
   f [] = Nothing
   f vorto =
      if "dri" `isSuffixOf` vorto || "gri" `isSuffixOf` vorto 
         || "dru" `isSuffixOf` vorto || "gru" `isSuffixOf` vorto then do
         let
            restantaVorto = mAlA vorto
            sekva =
               if "dri" `isSuffixOf` vorto || "dru" `isSuffixOf` vorto then
                  KunigaSubstantivoN
               else
                  KunigaSubstantivoNN
         (bazo, _) <- apliki legiBazanVorton restantaVorto
         case bazo of
            BazaŜtupo vt | vt == KunigaSubstantivoN || vt == KunigaSubstantivoNN ->
               return (NebazaŜtupo (Malantaŭigita, [sekva]), restantaVorto)
            _ -> undefined
      else
         Nothing

legiBazanVorton :: Legilo MalinflektaŜtupo
legiBazanVorton = Legilo f where
   f [] = Nothing
   f vorto = bazaFinaĵoDe vorto
      & (>>= (\v -> if ĉuValidaVorto vorto then Just v else Nothing))
      & fmap (\v -> (BazaŜtupo v, vorto))

malinflekti :: String -> Maybe MalinflektitaVorto
malinflekti vorto =
   tuteMalinflekti2 [Ĉio] vorto
   & fmap (\(ŝtupoj, bazaVorto) ->
      let
         inflekcioj =
            init ŝtupoj
            & fmap (\case
               NebazaŜtupo (i, _) -> i
               _ -> undefined)
            & reverse
         bazaŜtupo = case last ŝtupoj of
            BazaŜtupo vt -> vt
            _ -> undefined
      in
      MalinflektitaVorto {ŝtupoj=inflekcioj, bazaTipo=bazaŜtupo, bazaVorto=bazaVorto})