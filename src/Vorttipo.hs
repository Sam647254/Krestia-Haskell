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
   | ReflekcioM
   | ReflekcioS
   | Malantaŭigita
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
   
   , ("io", Perfekto, [Verbo0, Verbo2, Verbo3, Verbo23])
   , ("ia", Hipotezo, [Verbo0, Verbo2, Verbo3, Verbo23])
   , ("ela", Intenco, [Verbo0, Verbo1, Verbo12, Verbo13, Verbo123, Verbo2, Verbo23, Verbo3])
   , ("ea", Ĝerundo, [Verbo0, Verbo12, Verbo123, Verbo2, Verbo3, Verbo23])
   , ("ro", Perfekto, [Verbo12, Verbo123])
   , ("o", Perfekto, [Verbo1, Verbo13])
   , ("e", Hipotezo, [Verbo1, Verbo13])
   , ("ora", Desiderativo, [Verbo1, Verbo12, Verbo13, Verbo123])
   , ("ea", Imperativo, [Verbo1, Verbo13])
   , ("ri", Imperativo, [Verbo12, Verbo123])
   , ("ie", Optativo, [Verbo1, Verbo13, Verbo3])
   , ("ra", Optativo, [Verbo2])
   , ("ri", Optativo, [Verbo23])
   , ("a", Hortativo, [Verbo1, Verbo13])
   , ("etie", Argumento1, [Verbo1, Verbo12, Verbo13, Verbo123])
   , ("onia", Argumento2, [Verbo12, Verbo2, Verbo123, Verbo23])
   , ("eri", Argumento3, [Verbo123, Verbo13, Verbo3, Verbo123])
   , ("mea", Ĝerundo, [Verbo1, Verbo13])
   , ("elim", Komenco, [Verbo0])
   , ("elit", Komenco, [Verbo12])
   , ("elis", Komenco, [Verbo1])
   , ("elish", Komenco, [Verbo13])
   , ("elip", Komenco, [Verbo123])
   , ("em", Parto1, [Verbo1])
   , ("ig", Parto1, [Verbo12])
   , ("ev", Parto1, [Verbo123])
   , ("es", Parto2, [Verbo12])
   , ("am", Parto2, [Verbo2])
   , ("on", Parto2, [Verbo23])
   , ("osh", Parto2, [Verbo123])
   , ("ut", Parto3, [Verbo123])
   , ("ig", Parto3, [Verbo23])
   , ("ris", Reflekcio, [Verbo12])
   , ("ish", Reflekcio, [Verbo123])
   , ("es", Reflekcio, [Verbo13])
   , ("is", ReflekcioS, [Verbo123])
   , ("im", ReflekcioM, [Verbo123])
   , ("rim", ReflekcioM, [Verbo12])
   , ("ret", Unue2, [Verbo12])
   , ("rop", Unue2, [Verbo123])
   , ("rup", Unue3, [Verbo123])
   , ("rosh", Unue3, [Verbo13])
   , ("riv", Unue3, [Verbo23])
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
   , (KunigaSubstantivoN, ["dro", "dru"])
   , (KunigaSubstantivoNN, ["gro", "gru"])
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

mAlA kunigaS =
   case reverse kunigaS of
      ('u' : r) -> reverse ('o' : r)
      ('i' : r) -> reverse ('e' : r)
      ('r' : r) -> reverse ('l' : r)
      _ -> undefined

bazaFinaĵoDe :: String -> Maybe Vorttipo
bazaFinaĵoDe vorto =
   case reverse vorto of
      ('s' : _) -> Just Verbo1
      ('t' : _) -> Just Verbo12
      ('h' : 's' : _) -> Just Verbo13
      ('p' : _) -> Just Verbo123
      ('l' : _) -> Just Modifanto
      rvorto ->
         (find (`isPrefixOf` rvorto) (reverse <$> finaĵojDeSubstantivoN) & fmap (const SubstantivoN))
         <|> (find (`isPrefixOf` rvorto) (reverse <$> finaĵojDeSubstantivoNN) & fmap (const SubstantivoNN))
         <|> (do
            case vorto of
               _ | "dre" `isSuffixOf` vorto -> return KunigaSubstantivoN
               _ | "gre" `isSuffixOf` vorto -> return KunigaSubstantivoNN
               _ -> Nothing)

semblas :: (Inflekcio, Vorttipo) -> Vorttipo -> Bool
semblas (Havado, _) Verbo1 = True
semblas (HavadoM, _) Verbo0 = True
semblas (Ĝerundo, _) SubstantivoNN = True
semblas (ĜerundoS, _) SubstantivoNN = True
semblas (Kvalito, _) SubstantivoNN = True
semblas (Translativo, _) Verbo1 = True
semblas (TranslativoM, _) Verbo0 = True
semblas (Argumento1, _) SubstantivoN = True
semblas (Argumento2, _) SubstantivoN = True
semblas (Argumento3, _) SubstantivoN = True
semblas (Parto1, Verbo1) Verbo0 = True
semblas (Parto1, Verbo12) Verbo2 = True
semblas (Parto1, Verbo123) Verbo23 = True
semblas (Parto2, Verbo2) Verbo0 = True
semblas (Parto2, Verbo12) Verbo1 = True
semblas (Parto2, Verbo123) Verbo13 = True
semblas (Parto3, Verbo3) Verbo0 = True
semblas (Parto3, Verbo23) Verbo2 = True
semblas (Parto3, Verbo123) Verbo12 = True
semblas (Komenco, vt) vt2 = vt == vt2
semblas (Reflekcio, Verbo12) Verbo1 = True
semblas (Unue2, vt) vt2 = vt == vt2
semblas (Unue3, vt) vt2 = vt == vt2
semblas _ _ = False

uzasPEsti :: Inflekcio -> Maybe Inflekcio
uzasPEsti ĜerundoS = Just Ĝerundo
uzasPEsti _ = Nothing

ĉuVerbo vorttipo =
   vorttipo == Verbo1 ||
   vorttipo == Verbo12