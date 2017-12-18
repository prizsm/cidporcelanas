{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Estoquemoldura where

import Import
import Database.Persist.Postgresql


formEstoquemoldura :: Form Estoquemoldura
formEstoquemoldura = renderDivs $ Estoquemoldura
    <$> areq intField  "Código:  "       Nothing
    <*> areq intField  "Quantidade:  "   Nothing
    <*> areq textField "Observação:  "   Nothing

getEstoquemolduraR :: Handler Html
getEstoquemolduraR = do
            (widget, enctype) <- generateFormPost formEstoquemoldura
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
                     <h2 align="center"> Cadastrar Estoque molduras
                     <form method=post action=@{EstoquemolduraR} enctype=#{enctype}>
                         ^{widget}
                         <input type="submit" class="botao2"  value="Cadastrar">
                |]
                

postEstoquemolduraR :: Handler Html
postEstoquemolduraR = do
            ((result, _), _) <- runFormPost formEstoquemoldura
            case result of
                FormSuccess estoquemoldura -> do
                    molid <- runDB $ insert estoquemoldura
                    redirect ListEstoquemolduraR
                _ -> redirect HomeR

getListEstoquemolduraR :: Handler Html
getListEstoquemolduraR = do
            molduras <- runDB $ selectList [] [Asc EstoquemolduraCodigo]
            defaultLayout $(whamletFile "templates/tabela6.hamlet")

postDelEstoquemolduraR :: EstoquemolduraId -> Handler Html
postDelEstoquemolduraR molid = do 
                runDB $ delete molid
                redirect ListEstoquemolduraR

getEditarQtdeEstoqueR :: EstoquemolduraId -> Handler Html
getEditarQtdeEstoqueR molid = do
    molduras <- runDB $ selectList [EstoquemolduraId ==. molid] []
    defaultLayout [whamlet|
        ^{menu3}
        $forall Entity molid estoquemoldura <- molduras
            <h2 align="center">Editar Estoque de molduras</h2>
                <tr>
                    <form action=@{EditarQtdeEstoqueR molid} method=post>
                        <td><span class="negrito">Código:</span> #{show (estoquemolduraCodigo estoquemoldura)}<br>
                        <td><span class="negrito">Quantidade:</span> <input type="number" name="estoquemolduraQuantidade" value=#{show (estoquemolduraQuantidade estoquemoldura)}><br>
                        <td><span class="negrito">Observação:</span> <input type="text" name="estoquemolduraObservacao" value=#{unpack (estoquemolduraObservacao estoquemoldura)}><br>
                        <td><input class="botao2" type="submit" value="Alterar">
    |]

postEditarQtdeEstoqueR :: EstoquemolduraId -> Handler Html
postEditarQtdeEstoqueR molid = do
            (estoquemolduraQuantidade, estoquemolduraObservacao) <- runInputPost $ (,)
                <$> ireq intField "estoquemolduraQuantidade"
                <*> ireq textField "estoquemolduraObservacao"
            _ <- runDB $ update molid [EstoquemolduraQuantidade =. estoquemolduraQuantidade, EstoquemolduraObservacao =. estoquemolduraObservacao]
            redirect ListEstoquemolduraR

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
