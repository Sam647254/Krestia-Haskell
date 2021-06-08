module Iloj where

import Malinflektado
import Control.Arrow
import Data.Function

igiEnTimeranTxt :: String -> Either String [String]
igiEnTimeranTxt eniro = do
   vortoj <- malinflektiTekston eniro
   dividitaj <- traverse dividiRadikon vortoj
   return (concat dividitaj)

malinflektiTekston :: String -> Either String [MalinflektitaVorto]
malinflektiTekston = words >>> traverse malinflekti