module Vorttipo where

import Data.List
import Data.Function
import qualified Data.Map.Strict as Map
import Control.Applicative
data Vorttipo
   = SubstantivoN
   | SubstantivoNN
   | KunigaSubstantivoN
   | KunigaSubstantivoNN
   | Rekordo
   | Verbo1
   | Verbo12
   | Verbo13
   | Verbo123
   | Verbo2
   | Verbo23
   | Verbo3
   | Verbo0
   | Lokokupilo
   | Modifanto
   | FremdaVorto
   | Cifero
   | Ĉio
   deriving (Show, Eq)

data Inflekcio
   = Difinito
   | Havaĵo
   | PEsti
   | AEstiA
   | AEstiMA
   | Ĝerundo
   | ĜerundoS
   | Havado
   | Ekzistado
   | Translativo
   | Apartigita
   | Sola
   | Progresivo
   | Perfekto
   | Intenco
   | Hipotezo
   | Desiderativo
   | Imperativo
   | Hortativo
   | Optativo
   | Argumento1
   | Argumento2
   | Argumento3
   | Parto1
   | Parto2
   | Parto3
   | Unue2
   | Unue3
   | Komenco
   | Reflekcio
   | Antaŭigita
   | Kvalito
   | SolaFormo
   | FremdaVortoI
   | CiferoI
   deriving (Show, Eq)

type Finaĵo = String

finaĵojKajĴustajSekvaĵoj :: [(Finaĵo, Inflekcio, [Vorttipo])]
finaĵojKajĴustajSekvaĵoj =
   [ ("va", AEstiA, [SubstantivoN, SubstantivoNN])
   , ("ga", AEstiMA, [SubstantivoN, SubstantivoNN])
   , ("vra", ĜerundoS, [SubstantivoN, SubstantivoNN])
   , ("re", Kvalito, [SubstantivoN, SubstantivoNN])
   , ("ra", Sola, [SubstantivoN, SubstantivoNN])
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

finaĵojDePEsti :: [(Vorttipo, [String])]
finaĵojDePEsti =
   [ (SubstantivoN, ["paa", "po", "pu", "taa", "to", "tu", "kaa", "ko", "ku"])
   , (SubstantivoNN, ["maa", "mo", "mu", "naa", "no", "nu"])
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

pAlD substantivo =
   case reverse substantivo of
      ('a' : 'a' : r) -> reverse ('a' : r)
      ('o' : r) -> reverse ('e' : r)
      ('u' : r) -> reverse ('i' : r)
      _ -> undefined

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
uzasPEsti ĜerundoS = Just Ĝerundo
uzasPEsti _ = Nothing

ĉuVerbo vorttipo =
   vorttipo == Verbo1 ||
   vorttipo == Verbo12