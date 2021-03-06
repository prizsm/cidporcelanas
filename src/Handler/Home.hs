{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Home where

import Import
import Database.Persist.Postgresql
import Handler.Common

getHomeR :: Handler Html
getHomeR = do
        (widget, enctype) <- generateFormPost formFalecom
        defaultLayout $ do
        
            sess <- lookupSession "_ID"
            setTitle "Cidinha Porcelanas"
            addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
            addStylesheet $ StaticR css_estilo_css
            toWidgetHead [hamlet|
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
            |]
            $(whamletFile "templates/menu.hamlet")
            $(whamletFile "templates/principal.hamlet")
            [whamlet|
                <div id="contato1"><h1>FALE CONOSCO</h1><form method=post action=@{FalecomR} enctype=#{enctype}>
                    ^{widget}
                    <input class="botao2" type="submit" value="Enviar"></div>
            |]
            $(whamletFile "templates/endereco.hamlet")
            $(whamletFile "templates/footer.hamlet")
