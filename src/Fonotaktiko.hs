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

ĉuValidaVorto :: [Litero] -> Bool
ĉuValidaVorto vorto =
   case vorto of
      [Vokalo _] -> True
      [Vokalo _, Konsonanto _] -> True
      [Konsonanto _, Vokalo _] -> True
      [Konsonanto _, Vokalo _, Konsonanto _] -> True
      Vokalo _ : Konsonanto k : restanta ->
         ĉuValidaVorto (Konsonanto k: restanta)
      _ -> False