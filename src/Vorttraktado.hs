module Vorttraktado where

import Malinflektado
import Vorttipo
valencoDe :: MalinflektitaVorto -> Int
valencoDe vorto =
   let
      deInflekcioj =
         foldl
            (\ak sekva ->
               case sekva of
                  Havaĵo |
                     bazaTipo vorto == KunigaSubstantivoN ||
                     bazaTipo vorto == KunigaSubstantivoNN ->
                        ak - 1
                  Apartigita -> ak - 1
                  PEsti -> ak + 1
                  Havado -> ak + 1
                  Translativo -> ak + 1
                  Imperativo -> ak - 1
                  Hortativo -> ak - 1
                  Parto1 -> ak - 1
                  Parto2 -> ak - 1
                  Parto3 -> ak - 1
                  Reflekcio -> ak - 1
                  ReflekcioM -> ak - 2
                  ReflekcioS -> ak - 2
                  _ -> ak)
            0
            (ŝtupoj vorto)
      baza =
         case bazaTipo vorto of
            Verbo1 -> 1
            Verbo2 -> 1
            Verbo3 -> 1
            Verbo12 -> 2
            Verbo13 -> 2
            Verbo23 -> 2
            Verbo123 -> 3
            _ -> 0
   in
   baza + deInflekcioj

ĉuPredikato :: MalinflektitaVorto -> Bool
ĉuPredikato vorto =
   case ŝtupoj vorto of
      [] ->
         case bazaTipo vorto of
            Verbo0 -> True
            Verbo1 -> True
            Verbo12 -> True
            Verbo123 -> True
            Verbo13 -> True
            Verbo2 -> True
            Verbo23 -> True
            Verbo3 -> True
            _ -> False
      listo ->
         case last listo of
            Perfekto -> True
            Intenco -> True
            Desiderativo -> True
            Imperativo -> True
            Optativo -> True
            Hortativo -> True
            Hipotezo -> True
            Havado -> True
            HavadoM -> True
            Ekzistado -> True
            Translativo -> True
            TranslativoM -> True
            Parto1 -> True
            Parto2 -> True
            Parto3 -> True
            Reflekcio -> True
            ReflekcioM -> True
            ReflekcioS -> True
            Unue2 -> True
            Unue3 -> True
            PEsti -> True
            Sola -> True
            Komenco -> True
            _ -> False