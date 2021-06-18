module Sintaksanalizilo where

import Control.Monad.State.Lazy
import Malinflektado
import Control.Monad.Except
import Control.Monad.Identity

data Sintaksanalizilo = Sintaksanalizilo
   { eniro :: [MalinflektitaVorto]
   , frazoj :: [Frazo]
   }

data Frazo = Frazo
   { predikato :: MalinflektitaVorto
   , argumentoj :: [Argumento]
   }

newtype Argumento = Argumento MalinflektitaVorto

type SAStato = ExceptT String (State Sintaksanalizilo)

alportiSekvanVorton :: SAStato MalinflektitaVorto
alportiSekvanVorton = do
   stato <- get
   case eniro stato of
      [] -> throwError "Run out of words to read"
      (vorto : restanta) -> do
         put (stato { eniro = restanta })
         return vorto