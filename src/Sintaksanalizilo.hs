module Sintaksanalizilo where

import Control.Monad.State.Lazy
import Malinflektado
import Control.Monad.Except
import Control.Monad.Identity
import Vorttraktado
import Iloj
import Data.Function
import Data.Maybe (fromMaybe)
import Control.Arrow
import Vorttipo

data Sintaksanalizilo = Sintaksanalizilo
   { eniro :: [MalinflektitaVorto]
   , modifitajVortoj :: [ModifitaVorto]
   , atendantajArgumentoj :: [Argumento]
   , atendantajPredikatoj :: [Predikato]
   }

data Atributo
   = Atributo ModifitaVorto
   | AtributoKunAldonaĵoj MalinflektitaVorto [Argumento]
   deriving (Show)

data Modifanto
   = AntaŭModifanto Atributo
   | MalantaŭModifanto Atributo
   | Nemodifanto
   deriving (Show)

data ModifitaVorto = ModifitaVorto
   { vorto :: MalinflektitaVorto
   , modifantoj :: [Modifanto]
   }
   deriving (Show)

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

ĉuPovasModifi :: Modifanto -> MalinflektitaVorto -> Bool
ĉuPovasModifi modifanto v =
   let
      atributo =
         case modifanto of
            AntaŭModifanto a -> Just a
            MalantaŭModifanto a -> Just a
            _ -> Nothing
   in
   case atributo of
      Nothing -> False
      Just atributo ->
         case atributo of
            Atributo a ->
               if null $ ŝtupoj $ vorto a then
                  undefined
               else
                  case (last $ ŝtupoj $ vorto a, bazaTipo v) of
                     (AEstiA, SubstantivoN) -> True
                     (AEstiA, SubstantivoNN) -> True
                     (AEstiMA, SubstantivoN) -> True
                     (AEstiMA, SubstantivoNN) -> True
                     _ -> False
            AtributoKunAldonaĵoj aa _ -> undefined

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
   let a = execState (runExceptT legi1Ak) (Sintaksanalizilo {eniro=vortoj, modifitajVortoj=[],
      atendantajArgumentoj=[], atendantajPredikatoj=[]})
   legi2 a