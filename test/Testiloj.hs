module Testiloj where

import Malinflektado
import Sintaksanalizilo

praveMalinflekti :: String -> MalinflektitaVorto
praveMalinflekti eniro =
   case malinflekti eniro of
      Right malinflektitaVorto -> malinflektitaVorto
      Left eraro -> error eraro

praveMalinflektiĈiujn = map praveMalinflekti . words

modifitaVorto :: MalinflektitaVorto -> ModifitaVorto
modifitaVorto vorto =
   ModifitaVorto { vorto = vorto, modifantoj = [] }