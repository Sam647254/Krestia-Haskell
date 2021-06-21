module Sintaksanalizilo where

import Control.Monad.State.Lazy
import Malinflektado
import Control.Monad.Except
import Control.Monad.Identity
import Vorttraktado
import Iloj
import Data.Function

data Sintaksanalizilo = Sintaksanalizilo
   { eniro :: [MalinflektitaVorto]
   , atendantajArgumentoj :: [Argumento]
   , atendantajPredikatoj :: [Predikato]
   }

data Frazo = Frazo
   { predikato :: Predikato
   , argumentoj :: [Argumento]
   }
   deriving (Show)

data Rezulto = Rezulto
   { plenajFrazoj :: [Frazo]
   , restantajArgumentoj :: [Argumento]
   }
   deriving (Show)

data Aldonaĵo
   = Aldonaĵo MalinflektitaVorto [Argumento]
   | FrazaAldonaĵo Frazo
   deriving (Show)

newtype Argumento = Argumento MalinflektitaVorto
   deriving (Show)

newtype Predikato = Predikato MalinflektitaVorto
   deriving (Show)

type SAStato = ExceptT Eraro (State Sintaksanalizilo)

data Eraro
   = FinoDeEniro
   | Alia String
   deriving (Show)

alportiSekvanVorton :: SAStato MalinflektitaVorto
alportiSekvanVorton = do
   stato <- get
   case eniro stato of
      [] -> throwError FinoDeEniro
      (vorto : restanta) -> do
         put (stato { eniro = restanta })
         return vorto

vidiSekvanVorton :: SAStato (Maybe MalinflektitaVorto)
vidiSekvanVorton = do
   stato <- get
   case eniro stato of
      [] -> return Nothing
      (vorto : _) -> return (Just vorto)

aldoniAtendantanArgumenton :: SAStato ()
aldoniAtendantanArgumenton = do
   vorto <- alportiSekvanVorton
   let argumento = Argumento vorto
   modify (\stato ->
      stato { atendantajArgumentoj
         = argumento : atendantajArgumentoj stato })

legi1Ak :: SAStato ()
legi1Ak = do
   sekva <- vidiSekvanVorton
   case sekva of
      Nothing -> return ()
      Just vorto | ĉuArgumento vorto -> do
         aldoniAtendantanArgumenton
         legi1Ak
      _ -> undefined

legi2 :: Sintaksanalizilo -> Either String Rezulto
legi2 sintaksanalizilo = do
   return (Rezulto {plenajFrazoj=[],
      restantajArgumentoj=atendantajArgumentoj sintaksanalizilo})

legi :: String -> Either String Rezulto
legi eniraTeksto = do
   vortoj <- malinflektiTekston eniraTeksto
   let a = execState (runExceptT legi1Ak) (Sintaksanalizilo {eniro=vortoj, atendantajArgumentoj=[], atendantajPredikatoj=[]})
   legi2 a