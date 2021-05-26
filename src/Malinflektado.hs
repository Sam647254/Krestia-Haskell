{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
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

legiFinaĵon :: Legilo MalinflektaŜtupo
legiFinaĵon = Legilo f where
   f :: String -> Maybe (MalinflektaŜtupo, String)
   f [] = Nothing
   f vorto = (do
      (f, i, vt) <- find (\(finaĵo, _, _) -> finaĵo `isSuffixOf` vorto) finaĵojKajĴustajSekvaĵoj
      let restanta = take (length vorto - length f) vorto
      case uzasPEsti i of
         Just pi -> do
            (restantaj, _) <- malinflekti_ restanta
            case restantaj of
               (NebazaŜtupo (n, _) : _) ->
                  if n == PEsti then
                     Just (NebazaŜtupo (pi, vt), pAlD restanta)
                  else
                     Just (NebazaŜtupo (i, vt), restanta)
               _ -> Just (NebazaŜtupo (i, vt), restanta)
         Nothing -> Just (NebazaŜtupo (i, vt), restanta))
      <|> apliki legiPEsti vorto
      <|> apliki legiBazanVorton vorto

legiPEsti :: Legilo MalinflektaŜtupo
legiPEsti = Legilo f where
   f [] = Nothing
   f vorto = do
      (vorttipo, _) <-
         find (\(tipo, finaĵoj) -> any (`isSuffixOf` vorto) finaĵoj) finaĵojDePEsti
      return (NebazaŜtupo (PEsti, [vorttipo]), pAlD vorto)

legiBazanVorton :: Legilo MalinflektaŜtupo
legiBazanVorton = Legilo f where
   f [] = Nothing
   f vorto = bazaFinaĵoDe vorto
      & (>>= (\v -> if ĉuValidaVorto vorto then Just v else Nothing))
      & fmap (\v -> (BazaŜtupo v, vorto))

legiLastanŜtupon :: Legilo [MalinflektaŜtupo]
legiLastanŜtupon = do
   b <- legiBazanVorton
   return [b]

tuteMalinflekti :: String -> [Vorttipo] -> Maybe ([MalinflektaŜtupo], String)
tuteMalinflekti vorto pravajVorttipoj = do
   (sekvaŜtupo, restanta) <- apliki legiFinaĵon vorto
   case sekvaŜtupo of
      NebazaŜtupo (_, s) -> do
         (restantaj, lasta) <-
            tuteMalinflekti restanta s
            <|> apliki legiLastanŜtupon restanta
            <|> apliki legiLastanŜtupon vorto
         case restantaj of
            (NebazaŜtupo (_, vt) : _) ->
               if (not . null) (vt `intersect` s) then
                  return (sekvaŜtupo : restantaj, lasta)
               else do
                  (baza, lasta) <- apliki legiBazanVorton vorto
                  return ([baza], lasta)
            [BazaŜtupo vt] ->
               if vt `elem` s then
                  return (if lasta /= restanta then restantaj else sekvaŜtupo : restantaj, lasta)
               else do
                  (baza, lasta') <- apliki legiBazanVorton vorto
                  return ([baza], lasta)
            _ -> undefined
      BazaŜtupo v -> do
         guard (v `elem` pravajVorttipoj || pravajVorttipoj == [Ĉio])
         return ([sekvaŜtupo], restanta)

malinflekti_ :: String -> Maybe ([MalinflektaŜtupo], String)
malinflekti_ = (`tuteMalinflekti` [Ĉio])

malinflekti :: String -> Maybe MalinflektitaVorto
malinflekti vorto =
   tuteMalinflekti vorto [Ĉio]
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