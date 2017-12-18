{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    authRoute _ = Just LoginR
  
    makeLogger = return . appLogger
  
    isAuthorized HomeR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized FalecomR _ = return Authorized
    isAuthorized EnderecoR _ = return Authorized
    isAuthorized SobreR _ = return Authorized
    isAuthorized PortfolioR _ = return Authorized
    isAuthorized CanecasR _ = return Authorized
    isAuthorized XicarasR _ = return Authorized
    isAuthorized PotesR _ = return Authorized
    isAuthorized PratosR _ = return Authorized
    isAuthorized TacasR _ = return Authorized
    isAuthorized VasosR _ = return Authorized
    isAuthorized QuadroR _ = return Authorized
    isAuthorized PublicacoesR _ = return Authorized
    isAuthorized OutrosR _ = return Authorized
    isAuthorized OrcporcelanaR _ = return Authorized
    isAuthorized Orcporcelana1R _ = return Authorized
    isAuthorized (OrcporcelanafinalR orcporcid) _ = return Authorized
    isAuthorized (Orcporcelana1finalR orcporid) _ = return Authorized
    isAuthorized OrcmolduraR _ = return Authorized
    isAuthorized Orcmoldura1R _ = return Authorized
    isAuthorized ModelosMoldurasR _ = return Authorized
    isAuthorized (OrcmoldurafinalR orcmolid) _ = return Authorized
    isAuthorized (Orcmoldura1finalR orcmoid) _ = return Authorized

    isAuthorized _ _ = estaAutenticado

estaAutenticado = do
    msu <- lookupSession "_ID"
    case msu of
        Just _ -> return Authorized
        Nothing -> return AuthenticationRequired

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
