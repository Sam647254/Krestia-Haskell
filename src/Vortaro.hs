module Vortaro where

import Vorttipo (Vorttipo, Inflekcio)
import Data.List.Split (splitOn)
import Malinflektado

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
         radikoj = splitOn "," (partoj !! 3)
         noto =
            let n = partoj !! 4 in
            if null n then Nothing else Just n
      return
         (VortaraVorto
            { vorto=vorto
            , signifo=signifo
            , simplaSignifo=simplaSignifo
            , radikoj=radikoj
            , noto=noto
            , ujoj=ujoj
            , povasModifi=Nothing
            , aldonaĵoj=Nothing
            })