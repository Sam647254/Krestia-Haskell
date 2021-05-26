module Vorttipo where

import Data.List
import Data.Function
import Control.Applicative
data Vorttipo
   = SubstantivoN
   | SubstantivoNN
   | Verbo1
   | Verbo12
   | Verbo13
   | Verbo123
   | Ĉio
   deriving (Show, Eq)

data Inflekcio
   = Difinito
   | PredikativaEsti
   | AtributivaEstiA
   | AtributivaEstiMA
   | Ĝerundo
   | ĜerundoS
   | Progresivo
   | Perfekto
   | Komenco
   deriving (Show, Eq)

type Finaĵo = String

finaĵojKajĴustajSekvaĵoj :: [(Finaĵo, Inflekcio, [Vorttipo])]
finaĵojKajĴustajSekvaĵoj =
   [ ("va", AtributivaEstiA, [SubstantivoN, SubstantivoNN])
   , ("ga", AtributivaEstiMA, [SubstantivoN, SubstantivoNN])
   , ("ro", Perfekto, [Verbo12, Verbo123])
   , ("o", Perfekto, [Verbo1, Verbo13])
   , ("elit", Komenco, [Verbo12])
   , ("elis", Komenco, [Verbo1])
   , ("elish", Komenco, [Verbo13])
   , ("elip", Komenco, [Verbo123])
   ]

finaĵojDeSubstantivoN :: [String]
finaĵojDeSubstantivoN =
   [ "pa"
   , "pe"
   , "pi"
   , "ta"
   , "te"
   , "ti"
   , "ka"
   , "ke"
   , "ki"
   ]

finaĵojDeSubstantivoNN :: [String]
finaĵojDeSubstantivoNN =
   [ "ma"
   , "me"
   , "mi"
   , "na"
   , "ne"
   , "ni"
   ]

bazaFinaĵoDe :: String -> Maybe Vorttipo
bazaFinaĵoDe vorto =
   case reverse vorto of
      ('s' : _) -> Just Verbo1
      ('t' : _) -> Just Verbo12
      ('h' : 's' : _) -> Just Verbo13
      ('p' : _) -> Just Verbo123
      rvorto ->
         (find (`isPrefixOf` rvorto) (reverse <$> finaĵojDeSubstantivoN) & fmap (const SubstantivoN))
         <|> (find (`isPrefixOf` rvorto) (reverse <$> finaĵojDeSubstantivoNN) & fmap (const SubstantivoNN))

semblas :: (Inflekcio, Vorttipo) -> Vorttipo -> Bool
semblas (Progresivo, verbo) celaVerbo = verbo == celaVerbo && ĉuVerbo verbo
semblas (Perfekto, verbo) celaVerbo = verbo == celaVerbo && ĉuVerbo verbo
semblas (Komenco, verbo) celaVerbo = verbo == celaVerbo && ĉuVerbo verbo
semblas _ _ = False

uzasPEsti :: Inflekcio -> Maybe Inflekcio
uzasPEsti Ĝerundo = Just ĜerundoS
uzasPEsti _ = Nothing

ĉuVerbo vorttipo =
   vorttipo == Verbo1 ||
   vorttipo == Verbo12