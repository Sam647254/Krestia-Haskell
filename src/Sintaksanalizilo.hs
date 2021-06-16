module Sintaksanalizilo where

import Deque.Lazy
import Malinflektado

data Sintaksanalizilo = Sintaksanalizilo
   { eniro :: [String]
   , predikatoj :: [MalinflektitaVorto]
   }