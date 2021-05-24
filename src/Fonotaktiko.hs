module Fonotaktiko where

import qualified Data.Text as T
import Control.Arrow

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

ĉuValidaVorto :: Bool -> [Litero] -> Bool
ĉuValidaVorto komenco vorto =
   case vorto of
      [] -> not komenco
      [Konsonanto _, Vokalo _] -> not komenco
      [Konsonanto _, Vokalo _, Konsonanto _] -> True
      [Vokalo _, Konsonanto _, Vokalo _] -> komenco
      (Vokalo _ : restantaj) -> ĉuValidaVorto False restantaj
      (Konsonanto _ : Konsonanto _ : Vokalo _ : restantaj) ->
         ĉuValidaVorto False restantaj
      (Konsonanto _ : Vokalo _ : restantaj) ->
         ĉuValidaVorto False restantaj
      _ -> False