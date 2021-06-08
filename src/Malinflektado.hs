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
import Control.Applicative
import Control.Arrow
import Fonotaktiko
import Data.Char

newtype Legilo a = Legilo (String -> Either String (a, String))

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

apliki :: Legilo a -> String -> Either String (a, String)
apliki (Legilo legilo) = legilo

instance Functor Legilo where
   fmap = liftM

instance Applicative Legilo where
   pure valuo = Legilo (\eniro -> Right (valuo, eniro))
   (<*>) = ap

instance Alternative Legilo where
   empty = Legilo Left
   (<|>) legilo1 legilo2 = Legilo f where
      f eniro =
         let rezulto1 = apliki legilo1 eniro in
            case rezulto1 of
               Left _ -> apliki legilo2 eniro
               Right _ -> rezulto1

instance Monoid left => Alternative (Either left) where
  empty = Left mempty
  (<|>) either1 either2 =
     case either1 of
        Left _ -> either2
        _ -> either1

instance Monad Legilo where
   return = pure

   (>>=) :: Legilo a -> (a -> Legilo b) -> Legilo b
   (>>=) legilo legiloF = Legilo (\eniro -> do
      (valuo, restanta) <- apliki legilo eniro
      (valuo2, restanta2) <- apliki (legiloF valuo) restanta
      return (valuo2, restanta2))

proviTuteMalinflekti :: (Finaĵo, Inflekcio, [Vorttipo]) -> [Vorttipo] -> String -> Either String ([MalinflektaŜtupo], String)
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
                  restantajŜtupoj -> Left ("Kvalito must follow PEsti: " <> vorto)
            else do
               (restantajŜtupoj, bazaVorto) <- tuteMalinflekti2 vorttipoj restanta
               return (NebazaŜtupo (inflekcio, vorttipoj) : restantajŜtupoj, bazaVorto)
   else
      Left ("Cannot decompose " <> vorto)

tuteMalinflekti2Ak :: [(Finaĵo, Inflekcio, [Vorttipo])] -> [Vorttipo]
   -> [MalinflektaŜtupo] -> String -> Either String ([MalinflektaŜtupo], String)
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
      _ -> Left ("Invalid last step while decomposing " <> vorto))

tuteMalinflekti2Ak (sekva : restantaj) pravajVorttipoj ŝtupoj vorto =
   case proviTuteMalinflekti sekva pravajVorttipoj vorto of
      Right (restantajŜtupoj, bazaVorto) ->
         Right (restantajŜtupoj <> ŝtupoj, bazaVorto)
      Left _ -> tuteMalinflekti2Ak restantaj pravajVorttipoj ŝtupoj vorto

tuteMalinflekti2 :: [Vorttipo] -> String -> Either String ([MalinflektaŜtupo], String)
tuteMalinflekti2 pravajVorttipoj vorto = (do
   (bazaŜtupo, bazaVorto) <- apliki legiFremdanVorton vorto
   return ([bazaŜtupo], bazaVorto))
   <|> tuteMalinflekti2Ak finaĵojKajĴustajSekvaĵoj pravajVorttipoj [] vorto

malinflekti2 :: String -> Either String ([MalinflektaŜtupo], String)
malinflekti2 = tuteMalinflekti2 [Ĉio]

legiPEsti :: Legilo MalinflektaŜtupo
legiPEsti = Legilo f where
   f [] = Left "Empty string"
   f vorto = do
      case find (\(tipo, finaĵoj) -> any (`isSuffixOf` vorto) finaĵoj) finaĵojDePEsti of
         Just (vorttipo, _) -> Right (NebazaŜtupo (PEsti, [vorttipo]), pAlD vorto)
         Nothing -> Left ("Cannot read PEsti from " <> vorto)

dividiRadikon :: MalinflektitaVorto -> Either String [String]
dividiRadikon vorto =
   let
      bazo = bazaVorto vorto
      radiko =
         case bazaTipo vorto of
            SubstantivoN -> bazo
            SubstantivoNN -> bazo
            FremdaVorto -> bazo
            KunigaSubstantivoN -> take (length bazo - 3) bazo
            KunigaSubstantivoNN -> take (length bazo - 3) bazo
            Verbo13 -> take (length bazo - 2) bazo
            Rekordo -> take (length bazo - 2) bazo
            _ -> init bazo
      finaĵoj = map show (ŝtupoj vorto)
      bazaVorttipo =
         [show (bazaTipo vorto) | not (bazaTipo vorto == SubstantivoN || bazaTipo vorto == SubstantivoN)]
   in do
      silaboj <- dividi bazo
      return (if bazaTipo vorto == FremdaVorto then
         ["["] <> silaboj <> ["]"]
      else
         silaboj <> bazaVorttipo <> finaĵoj)

legiAntaŭigita :: Legilo MalinflektaŜtupo
legiAntaŭigita = Legilo f where
   f [] = Left "Empty string"
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
         Left ("Cannot read Antaŭigita from " <> vorto)

legiBazanVorton :: Legilo MalinflektaŜtupo
legiBazanVorton = Legilo f where
   f [] = Left "Empty string passed to legiBazanVorton"
   f vorto = case bazaFinaĵoDe vorto of
      Just v ->
         if ĉuValidaVorto vorto then
            Right (BazaŜtupo v, vorto)
         else
            Left (vorto <> " is not a valid base word")
      Nothing -> Left (vorto <> " is not a base word")

legiFremdanVorton :: Legilo MalinflektaŜtupo
legiFremdanVorton = Legilo f where
   f vorto | isUpper (head vorto) = Right (BazaŜtupo FremdaVorto, vorto)
   f _ = Left "Cannot read FremdaVorto"

malinflekti :: String -> Either String MalinflektitaVorto
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