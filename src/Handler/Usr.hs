{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Usr where

import Import
import Database.Persist.Postgresql


formUser :: Form Usr
formUser = renderDivs $ Usr
    <$> areq emailField    "E-mail"  Nothing
    <*> areq passwordField "Senha"   Nothing

getLoginR :: Handler Html
getLoginR = do
    (widget,enctype)<- generateFormPost formUser
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu2.hamlet")
        [whamlet|
            <div class="orcamento2"><h2>Faça seu Login</h2>
                <form action=@{LoginR} method=post enctype=#{enctype}>
                    ^{widget}
                    <input class="botao2" type="submit" value="Logar">
        |]
        $(whamletFile "templates/footer.hamlet")

getUsrR :: Handler Html
getUsrR = do
    (widget,enctype)<- generateFormPost formUser
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu3.hamlet")
        [whamlet|
            <div id="contato1"><h2>Cadastrar Novo Usuário
                <form action=@{UsrR} method=post enctype=#{enctype}>
                    ^{widget}
                    <input class="botao2" type="submit" value="Cadastrar">
        |]

postUsrR :: Handler Html
postUsrR = do
    ((resultado,_),_)<- runFormPost formUser
    case resultado of
        FormSuccess user -> do
            uid <- runDB $ insert user
            redirect ListUsrR
        _ -> redirect AdmR

getListUsrR :: Handler Html
getListUsrR = do
            usuarios <- runDB $ selectList [] [Asc UsrEmail]
            defaultLayout $(whamletFile "templates/tabela2.hamlet")

postDelUsrR :: UsrId -> Handler Html
postDelUsrR uid = do 
                runDB $ delete uid
                redirect ListUsrR

-- ROTA DE AUTENTICACAO
postLoginR :: Handler Html
postLoginR = do
    ((resultado,_),_)<- runFormPost formUser
    case resultado of
        FormSuccess user -> do
            usr <- runDB $ selectFirst [UsrEmail ==. (usrEmail user),
                                    UsrSenha ==. (usrSenha user)] []
            case usr of
                Nothing -> redirect LoginR
                Just (Entity uid _) -> do
                    setSession "_ID" (pack $ show uid)
                    redirect AdmR
        _ -> redirect HomeR
        
getPerfilR :: Handler Html
getPerfilR = do
    userId <- lookupSession "_ID"
    redirect AdmR
    -- defaultLayout [whamlet|
    --     <h1> Logadoooo!!!! #{show userId}
    -- |]

getLogoutR :: Handler Html
getLogoutR = do
    deleteSession "_ID"
    redirect HomeR
    
menu2 :: Widget
menu2 = do
    addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
    addStylesheet $ StaticR css_estilo_css
    toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
    |]
    $(whamletFile "templates/menu2.hamlet")

menu3 :: Widget
menu3 = do
    addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
    addStylesheet $ StaticR css_estilo_css
    toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
    |]
    $(whamletFile "templates/menu3.hamlet")

rodape :: Widget
rodape = do
    $(whamletFile "templates/footer.hamlet")
