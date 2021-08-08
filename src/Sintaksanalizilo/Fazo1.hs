module Sintaksanalizilo.Fazo1 where

import Malinflektado
import Sintaksanalizilo
import Control.Monad.Except
import Control.Monad.State.Lazy
import Iloj
import Vorttipo
import Data.List (partition)

data SAFazo1Stato = SAFazo1Stato
   { nelegitajVortoj :: [MalinflektitaVorto]
   , atendantajModifantoj :: [Modifanto]
   , legitajVortoj :: [ModifitaVorto]
   }

newtype SAFazo1Rezulto = SAFazo1Rezulto
   { modifitajVortoj :: [ModifitaVorto] }
   deriving (Show)

type F1Stato = ExceptT Eraro (State SAFazo1Stato)

igiEnModifitanVorto :: MalinflektitaVorto -> ModifitaVorto
igiEnModifitanVorto vorto = ModifitaVorto {vorto=vorto, modifantoj=[]}

igiEnModifanton :: MalinflektitaVorto -> Modifanto
igiEnModifanton vorto =
   case (ŝtupoj vorto, bazaTipo vorto) of
      ([], Modifanto) -> undefined
      ([], _) -> Nemodifanto
      _ ->
         case (last (ŝtupoj vorto), bazaTipo vorto) of
            (_, Modifanto) -> undefined
            (AEstiA, SubstantivoN) -> AntaŭModifanto $ Atributo $ igiEnModifitanVorto vorto
            (AEstiA, SubstantivoNN) -> AntaŭModifanto $ Atributo $ igiEnModifitanVorto vorto
            (AEstiMA, SubstantivoN) -> MalantaŭModifanto $ Atributo $ igiEnModifitanVorto vorto
            (AEstiMA, SubstantivoNN) -> MalantaŭModifanto $ Atributo $ igiEnModifitanVorto vorto
            _ -> Nemodifanto
            

alportiSekvanVorton :: F1Stato (Maybe MalinflektitaVorto)
alportiSekvanVorton = do
   stato <- get
   case nelegitajVortoj stato of
      [] -> return Nothing
      (vorto : restanta) -> do
         put (stato { nelegitajVortoj = restanta})
         return (Just vorto)

aldoniModifanton' :: [ModifitaVorto] -> Modifanto -> [ModifitaVorto]
aldoniModifanton' vortoj modifanto =
   case vortoj of
      [] -> error ("No word for " <> show modifanto <> " to modify")
      (sekva : restanta) ->
         if modifanto `ĉuPovasModifi` vorto sekva then
            let
               novaVorto = sekva { modifantoj = modifanto : modifantoj sekva }
            in
            novaVorto : restanta
         else
            sekva : aldoniModifanton' restanta modifanto

aldoniModifanton :: Modifanto -> F1Stato ()
aldoniModifanton modifanto = modify (\stato ->
   let novajVortoj = aldoniModifanton' (legitajVortoj stato) modifanto in
   stato { legitajVortoj = novajVortoj })

atendigiVorton :: MalinflektitaVorto -> F1Stato ()
atendigiVorton vorto = modify (\stato ->
   let
      (uzotajModifantoj, restantajModifantoj) =
         partition (`ĉuPovasModifi` vorto) (atendantajModifantoj stato)
   in
   stato { legitajVortoj =
      ModifitaVorto {vorto=vorto, modifantoj=reverse uzotajModifantoj} : legitajVortoj stato,
      atendantajModifantoj = restantajModifantoj })

atendigiModifanton :: Modifanto -> F1Stato ()
atendigiModifanton modifanto = modify (\stato ->
   stato { atendantajModifantoj = modifanto : atendantajModifantoj stato })

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
               aldoniModifanton modifanto
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