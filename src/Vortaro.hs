{-# LANGUAGE LambdaCase #-}
module Vortaro where
import Data.List.Split (splitOn)
import Malinflektado
import Vorttipo

dosiero :: String
dosiero = "vortaro.kv"

ŝarĝiVortaron :: IO Vortaro
ŝarĝiVortaron = do
   enhavo <- readFile dosiero
   let vicoj = lines enhavo
   case traverse ŝarĝiVorton vicoj of
      Right vortaro -> return (Vortaro {vortoj=vortaro})
      Left eraro -> error eraro

data VortaraVorto = VortaraVorto
   { vorto :: String
   , signifo :: String
   , simplaSignifo :: String
   , radikoj :: [String]
   , noto :: Maybe String
   , ujoj :: Maybe [String]
   , povasModifi :: Maybe [Vorttipo]
   , aldonaĵoj :: Maybe [Inflekcio]
   }
   deriving Show

newtype Vortaro = Vortaro { vortoj :: [VortaraVorto] } deriving Show

ŝarĝiVorton :: String -> Either String VortaraVorto
ŝarĝiVorton vico = do
   let
      partoj = splitOn "|" vico
      vorto = head partoj
   malinflektitaVorto <- malinflekti vorto
   if length (ŝtupoj malinflektitaVorto) > 1 then
      Left (vorto <> " is not a dictionary word")
   else do
      let
         (signifo, ujoj) =
            if ĉuVerbo malinflektitaVorto then
               let signifoPartoj = splitOn "^" (partoj !! 1) in
               (head signifoPartoj, Just (tail signifoPartoj))
            else
               (partoj !! 1, Nothing)
         simplaSignifo = partoj !! 2
         radikoj = if null (partoj !! 3) then [] else splitOn "," (partoj !! 3)
         noto =
            let n = partoj !! 4 in
            if null n then Nothing else Just n
         povasModifi =
            if length partoj >= 6 then
               let p = partoj !! 5 in
               Just (map alVorttipo p)
            else
               Nothing
         aldonaĵoj =
            if length partoj == 7 then
               let p = partoj !! 6 in
               Just (concatMap alInflekcio p)
            else
               Nothing
      return
         (VortaraVorto
            { vorto=vorto
            , signifo=signifo
            , simplaSignifo=simplaSignifo
            , radikoj=radikoj
            , noto=noto
            , ujoj=ujoj
            , povasModifi=povasModifi
            , aldonaĵoj=Nothing
            })

alVorttipo :: Char -> Vorttipo
alVorttipo = \case
   'N' -> SubstantivoN
   'n' -> SubstantivoNN
   '0' -> Verbo0
   '1' -> Verbo1
   '2' -> Verbo12
   '3' -> Verbo123
   '4' -> Verbo2
   '5' -> Verbo23
   '6' -> Verbo3
   '7' -> Verbo13
   'L' -> Rekordo
   'E' -> KunigaSubstantivoN
   'P' -> KunigaSubstantivoNN
   'Q' -> Lokokupilo
   'F' -> FremdaVorto
   'C' -> TerminaCifero
   'c' -> NeterminaCifero
   _ -> undefined

alInflekcio :: Char -> [Inflekcio]
alInflekcio = \case
   'D' -> [Difinito]
   'H' -> [Havaĵo]
   'P' -> [Progresivo]
   'p' -> [Perfekto]
   'I' -> [Intenco]
   'd' -> [Desiderativo]
   'E' -> [PEsti]
   'A' -> [AEstiA, AEstiMA]
   'h' -> [Havado, HavadoM]
   'i' -> [Imperativo]
   '1' -> [Argumento1]
   '2' -> [Argumento2]
   '3' -> [Argumento3]
   'e' -> [Ekzistado]
   't' -> [Hortativo]
   'T' -> [Translativo, TranslativoM]
   'Ĝ' -> [Ĝerundo]
   'ĝ' -> [ĜerundoS]
   'U' -> [Parto1]
   'J' -> [Parto2]
   'O' -> [Parto3]
   'S' -> [SolaFormo]
   'R' -> [Reflekcio, ReflekcioM, ReflekcioS]
   '4' -> [Unue2]
   '5' -> [Unue3]
   'o' -> [Optativo]
   'K' -> [Kvalito]
   'n' -> [Hipotezo]
   'X' -> [Apartigita]
   '@' -> [FremdaVortoI]
   '#' -> [CiferoI]
   '&' -> [Predikato]
   _ -> undefined