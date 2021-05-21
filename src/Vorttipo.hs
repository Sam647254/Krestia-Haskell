module Vorttipo
   ( Vorttipo(..)
   , Inflekcio(..)
   , finaĵojKajĴustajSekvaĵoj
   ) where

data Vorttipo
   = Verbo1
   | Verbo12
   deriving (Show, Eq)

data Inflekcio
   = Progresivo
   | Perfekto
   | Komenco
   deriving (Show, Eq)

type Finaĵo = String

finaĵojKajĴustajSekvaĵoj :: [(Finaĵo, Inflekcio, Vorttipo)]
finaĵojKajĴustajSekvaĵoj =
   [ ("ro", Perfekto, Verbo12)
   , ("o", Perfekto, Verbo1)
   , ("elit", Komenco, Verbo12)
   , ("elis", Komenco, Verbo1)
   ]