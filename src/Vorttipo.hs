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
   | HavadoM
   | Ekzistado
   | Translativo
   | TranslativoM
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
   , ("rem", HavadoM, [SubstantivoN, SubstantivoNN])
   , ("res", Havado, [SubstantivoN, SubstantivoNN])
   , ("rim", Ekzistado, [SubstantivoN, SubstantivoNN])
   , ("lam", TranslativoM, [SubstantivoN, SubstantivoNN])
   , ("las", Translativo, [SubstantivoN, SubstantivoNN])
   
   , ("io", Perfekto, [Verbo0])
   , ("ia", Hipotezo, [Verbo0])
   , ("ela", Intenco, [Verbo0, Verbo1, Verbo12, Verbo13, Verbo123, Verbo2, Verbo23, Verbo3])
   , ("ea", Ĝerundo, [Verbo0])
   , ("ro", Perfekto, [Verbo12, Verbo123])
   , ("o", Perfekto, [Verbo1, Verbo13])
   , ("e", Hipotezo, [Verbo1, Verbo13])
   , ("ora", Desiderativo, [Verbo1, Verbo12, Verbo13, Verbo123])
   , ("ea", Imperativo, [Verbo1, Verbo13])
   , ("ie", Optativo, [Verbo1])
   , ("a", Hortativo, [Verbo1])
   , ("etie", Argumento1, [Verbo1, Verbo12, Verbo13, Verbo123])
   , ("mea", Ĝerundo, [Verbo1])
   , ("elim", Komenco, [Verbo0])
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
semblas (Havado, _) Verbo1 = True
semblas (HavadoM, _) Verbo0 = True
semblas (Ĝerundo, _) SubstantivoNN = True
semblas (ĜerundoS, _) SubstantivoNN = True
semblas (Kvalito, _) SubstantivoNN = True
semblas (Argumento1, _) SubstantivoN = True
semblas (Argumento2, _) SubstantivoN = True
semblas (Argumento3, _) SubstantivoN = True
semblas (_, vt1) vt2 = vt1 == vt2

uzasPEsti :: Inflekcio -> Maybe Inflekcio
uzasPEsti ĜerundoS = Just Ĝerundo
uzasPEsti _ = Nothing

ĉuVerbo vorttipo =
   vorttipo == Verbo1 ||
   vorttipo == Verbo12