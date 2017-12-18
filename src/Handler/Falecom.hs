{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Falecom where

import Import
import Database.Persist.Postgresql
import Handler.Common

getFalecomR :: Handler Html
getFalecomR = do
            (widget, enctype) <- generateFormPost formFalecom
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
                    <div id="contato1"><h1>FALE CONOSCO</h1><form method=post action=@{FalecomR} enctype=#{enctype}>
                        ^{widget}
                        <input class="botao2" type="submit" value="Enviar">
                |]
                $(whamletFile "templates/footer.hamlet")

postFalecomR :: Handler Html
postFalecomR = do
            ((result, _), _) <- runFormPost formFalecom
            case result of
                FormSuccess falecom -> do
                    fcid <- runDB $ insert falecom
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
                            <h2 align="center"> Contato enviado com sucesso!!<br><br><br>
                            <a href="@{HomeR}">Voltar a página principal</a>
                        |]
                        $(whamletFile "templates/footer.hamlet")
                _ -> redirect HomeR

getListFalecomR :: Handler Html
getListFalecomR = do
            contatos <- runDB $ selectList [] [Asc FalecomNome]
            defaultLayout $(whamletFile "templates/tabela1.hamlet")
                
postDelFalecomR :: FalecomId -> Handler Html
postDelFalecomR fcid = do 
                runDB $ delete fcid
                redirect ListFalecomR

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
