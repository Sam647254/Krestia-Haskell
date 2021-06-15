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