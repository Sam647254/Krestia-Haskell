module Vorttipo where

data Vorttipo
   = Verbo1
   | Verbo12
   | Ĉio
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

bazaFinaĵoDe :: String -> Maybe Vorttipo
bazaFinaĵoDe vorto =
   case reverse vorto of
      ('s' : _) -> Just Verbo1
      ('t' : _) -> Just Verbo12