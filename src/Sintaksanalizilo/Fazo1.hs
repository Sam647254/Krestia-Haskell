module Sintaksanalizilo.Fazo1 where

import Malinflektado
import Sintaksanalizilo
import Control.Monad.Except
import Control.Monad.State.Lazy
import Iloj
import Vorttipo

data SAFazo1Stato = SAFazo1Stato
   { nelegitajVortoj :: [MalinflektitaVorto]
   , atendantajModifantoj :: [Modifanto]
   , legitajVortoj :: [ModifitaVorto]
   }

newtype SAFazo1Rezulto = SAFazo1Rezulto
   { modifitajVortoj :: [ModifitaVorto] }
   deriving (Show)

type F1Stato = ExceptT Eraro (State SAFazo1Stato)

igiEnModifanton :: MalinflektitaVorto -> Modifanto
igiEnModifanton vorto =
   case (ŝtupoj vorto, bazaTipo vorto) of
      ([], Modifanto) -> undefined
      ([], _) -> Nemodifanto
      _ ->
         case (last (ŝtupoj vorto), bazaTipo vorto) of
            (_, Modifanto) -> undefined
            (AEstiA, SubstantivoN) -> AntaŭModifanto (Atributo vorto)
            (AEstiA, SubstantivoNN) -> AntaŭModifanto (Atributo vorto)
            (AEstiMA, SubstantivoN) -> MalantaŭModifanto (Atributo vorto)
            (AEstiMA, SubstantivoNN) -> MalantaŭModifanto (Atributo vorto)
            _ -> Nemodifanto
            

alportiSekvanVorton :: F1Stato (Maybe MalinflektitaVorto)
alportiSekvanVorton = do
   stato <- get
   case nelegitajVortoj stato of
      [] -> return Nothing
      (vorto : restanta) -> do
         put (stato { nelegitajVortoj = restanta})
         return (Just vorto)

aldoniModifanton :: [ModifitaVorto] -> Modifanto -> [ModifitaVorto]
aldoniModifanton vortoj modifanto =
   case vortoj of
      [] -> error ("No word for " <> show modifanto <> "to modify")
      (sekva : restanta) ->
         if modifanto `ĉuPovasModifi` vorto sekva then
            let
               novaVorto = sekva { modifantoj = modifanto : modifantoj sekva }
            in
            novaVorto : restanta
         else
            sekva : aldoniModifanton restanta modifanto

atendigiVorton :: MalinflektitaVorto -> F1Stato ()
atendigiVorton vorto = modify (\stato ->
   stato { legitajVortoj =
      ModifitaVorto {vorto=vorto, modifantoj=reverse (atendantajModifantoj stato)} : legitajVortoj stato})

atendigiModifanton :: Modifanto -> F1Stato ()
atendigiModifanton modifanto = modify (\stato ->
   stato { atendantajModifantoj = modifanto : atendantajModifantoj stato})

trakti1Ak :: F1Stato ()
trakti1Ak = do
   sekvaVorto <- Sintaksanalizilo.Fazo1.alportiSekvanVorton
   case sekvaVorto of
      Nothing -> return ()
      Just vorto -> do
         let modifanto = igiEnModifanton vorto
         case modifanto of
            AntaŭModifanto _ ->
               atendigiModifanton modifanto
            MalantaŭModifanto _ ->
               undefined
            Nemodifanto -> do
               atendigiVorton vorto
         trakti1Ak

trakti1 :: [MalinflektitaVorto] -> SAFazo1Rezulto
trakti1 vortoj = do
   let
      rezulto = execState (runExceptT trakti1Ak)
         (SAFazo1Stato {nelegitajVortoj=vortoj, atendantajModifantoj=[], legitajVortoj=[]})
   SAFazo1Rezulto {Sintaksanalizilo.Fazo1.modifitajVortoj=reverse (legitajVortoj rezulto)}

legi1 :: String -> Either String SAFazo1Rezulto
legi1 eniraTeksto = do
   vortoj <- malinflektiTekston eniraTeksto
   return (trakti1 vortoj)