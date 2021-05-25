module Fonotaktiko where

import qualified Data.Text as T
import Control.Arrow
import Data.Function

data Litero
   = Konsonanto Char
   | Vokalo Char
   deriving (Eq)

normaligi :: String -> String
normaligi =
   T.pack
   >>> T.replace (T.pack "aa") (T.pack "ɒ")
   >>> T.replace (T.pack "sh") (T.pack "ʃ")
   >>> T.toLower
   >>> T.unpack

kAŭV :: Char -> Litero
kAŭV 'a' = Vokalo 'a'
kAŭV 'e' = Vokalo 'e'
kAŭV 'i' = Vokalo 'i'
kAŭV 'ɒ' = Vokalo 'ɒ'
kAŭV 'o' = Vokalo 'o'
kAŭV 'u' = Vokalo 'u'
kAŭV k = Konsonanto k

kategorigi :: String -> [Litero]
kategorigi = map kAŭV

ĉuValidaVorto :: String -> Bool
ĉuValidaVorto vorto =
   case dividi vorto of
      Right _ -> True
      Left _ -> False

dividi :: String -> Either String [String]
dividi vorto =
   let
      literoj = normaligi vorto & kategorigi
      gardi True _ = return ()
      gardi False mesaĝo = Left mesaĝo
      dividi_ komenco literoj =
         case literoj of
            [] -> do
               gardi (not komenco) "La eniro estas malplena"
               return []
            [Vokalo v] -> Right [[v]]
            [Konsonanto k1, Konsonanto k2, Vokalo v, Konsonanto kf] -> do
               gardi komenco "Vorto ne rajtas komenci per du konsonantoj"
               return [[k1, k2, v, kf]]
            [Konsonanto k1, Konsonanto k2, Vokalo v] -> do
               gardi komenco "Vorto ne rajtas komenci per du konsonantoj"
               return [[k1, k2, v]]
            [Konsonanto k1, Vokalo v, Konsonanto kf] -> Right [[k1, v, kf]]
            [Konsonanto k1, Vokalo v] -> Right [[k1, v]]
            [Vokalo v, Konsonanto kf] -> Right [[v, kf]]
            (Konsonanto k1 : Konsonanto k2 : Vokalo v :
               Konsonanto kf : Konsonanto kk2 : restantaj) -> do
               gardi komenco "Vorto ne rajtas komenci per du konsonantoj"
               restanta <- dividi_ False (Konsonanto kk2 : restantaj)
               return ([k1, k2, v, kf] : restanta)
            (Konsonanto k1 : Konsonanto k2 : Vokalo v : Konsonanto kf :
               Vokalo v2 : restantaj) -> do
               gardi komenco "Vorto ne rajtas komenci per du konsonantoj"
               restanta <- dividi_ False (Konsonanto kf : Vokalo v2 : restantaj)
               return ([k1, k2, v] : restanta)
            (Konsonanto k1 : Konsonanto k2 : Vokalo v : Vokalo v2 : restantaj) -> do
               gardi komenco "Vorto ne rajtas komenci per du konsonantoj"
               gardi (v /= v2) "Vokalo ne rajtas aperi dufoje"
               restanta <- dividi_ False (Vokalo v2 : restantaj)
               return ([k1, k2, v] : restanta)
            (Konsonanto k1 : Vokalo v : Konsonanto kf : Konsonanto kk2 : restantaj) -> do
               restanta <- dividi_ False (Konsonanto kk2 : restantaj)
               return ([k1, v, kf] : restanta)
            (Konsonanto k1 : Vokalo v : Konsonanto kk2 : Vokalo v2 : restantaj) -> do
               restanta <- dividi_ False (Konsonanto kk2 : Vokalo v2 : restantaj)
               return ([k1, v] : restanta)
            (Konsonanto k1 : Vokalo v : Vokalo v2 : restantaj) -> do
               gardi (v /= v2) "Vokalo ne rajtas aperi dufoje"
               restanta <- dividi_ False (Vokalo v2 : restantaj)
               return ([k1, v] : restanta)
            (Vokalo v : Konsonanto kf : Konsonanto kk2 : restantaj) -> do
               restanta <- dividi_ False (Konsonanto kk2 : restantaj)
               return ([v, kf] : restanta)
            (Vokalo v : Konsonanto kf : Vokalo v2 : restantaj) -> do
               restanta <- dividi_ False (Konsonanto kf : Vokalo v2 : restantaj)
               return ([v] : restanta)
            (Vokalo v : Vokalo v2 : restantaj) -> do
               gardi (v /= v2) "Vokalo ne rajtas aperi dufoje"
               restanta <- dividi_ False (Vokalo v2 : restantaj)
               return ([v] : restanta)
            _ -> Left "Nevalida vorto"
   in
   dividi_ True literoj