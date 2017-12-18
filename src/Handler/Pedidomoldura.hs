{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Pedidomoldura where

import Import
import Database.Persist.Postgresql

formPedidomoldura :: Form Pedidomoldura
formPedidomoldura = renderDivs $ Pedidomoldura
    <$> areq (selectFieldList status)  "Status: "            Nothing
    <*> areq textField  "Data de Entrada: "                  Nothing
    <*> areq textField "Nome do Cliente: "                   Nothing
    <*> areq textField  "Telefone: "                         Nothing
    <*> areq doubleField  "Lado 1: "                         (Just 0)
    <*> areq doubleField  "Lado 2: "                         (Just 0)
    <*> areq intField  "Quantidade: "                        (Just 0)
    <*> areq textField  "Modelo: "                           Nothing
    <*> areq textField  "Paspatur: "                         Nothing
    <*> areq (selectFieldList vidro)  "Vidro: "              Nothing
    <*> areq textField  "Observação: "                       Nothing
    <*> areq doubleField  "Valor Final: "                    (Just 0)

status :: [(Text, Text)]
status = [("Em Andamento", "Em Andamento"), ("Finalizado", "Finalizado")]

vidro :: [(Text, Text)]
vidro = [("Vidro Comum", "Vidro Comum"), ("Vidro Antireflexo", "Vidro Antireflexo"), ("Sem Vidro", "Sem Vidro"), ("Espelho", "Espelho")]

getPedidomolduraR :: Handler Html
getPedidomolduraR = do
            (widget, enctype) <- generateFormPost formPedidomoldura
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
                     <h2 align="center"> Cadastrar Pedido molduras
                     <form method=post action=@{PedidomolduraR} enctype=#{enctype}>
                         ^{widget}
                         <input type="submit" class="botao2" value="Cadastrar">
                |]

postPedidomolduraR :: Handler Html
postPedidomolduraR = do
            ((result, _), _) <- runFormPost formPedidomoldura
            case result of
                FormSuccess pedidomoldura -> do
                    pedmolid <- runDB $ insert pedidomoldura
                    redirect ListPedidomolduraR
                _ -> redirect HomeR

getEditarQtdePedidoR :: PedidomolduraId -> Handler Html
getEditarQtdePedidoR pedmolid = do
    molduras <- runDB $ selectList [PedidomolduraId ==. pedmolid] []
    defaultLayout [whamlet|
        ^{menu3}
        $forall Entity pedmolid pedidomoldura <- molduras
            <h2 align="center">Editar Pedido de molduras</h2>
                <tr>
                    <form action=@{EditarQtdePedidoR pedmolid} method=post>
                        <td><span class="negrito">Status: </span><select name="status"><option value="Em Andamento">Em Andamento</option><option value="Finalizado">Finalizado</option></select><br>
                        <td><span class="negrito">Data de Entrada:</span> #{unpack (pedidomolduraEntrada  pedidomoldura)}<br>
                        <td><span class="negrito">Nome Cliente: </span> #{unpack (pedidomolduraClientenome     pedidomoldura)}<br>
                        <td><span class="negrito">Telefone:</span> #{unpack (pedidomolduraClientetelefone pedidomoldura)}<br>
                        <td><span class="negrito">Lado 1:</span> #{show (pedidomolduraLado1  pedidomoldura)}<br>
                        <td><span class="negrito">Lado 2:</span> #{show (pedidomolduraLado2  pedidomoldura)}<br>
                        <td><span class="negrito">Quantidade:</span> #{show (pedidomolduraQuantidade  pedidomoldura)}<br>
                        <td><span class="negrito">Modelo:</span> #{unpack (pedidomolduraModelo  pedidomoldura)}<br>
                        <td><span class="negrito">Paspatur:</span> #{unpack (pedidomolduraPaspatour  pedidomoldura)}<br>
                        <td><span class="negrito">Vidro:</span> #{unpack (pedidomolduraVidro  pedidomoldura)}<br>
                        <td><span class="negrito">Observação:</span> <input type="text" name="pedidomolduraObservacao" value=#{unpack (pedidomolduraObservacao pedidomoldura)}><br>
                        <td><span class="negrito">Valor Final: </span>#{show (pedidomolduraValorfinal  pedidomoldura)}<br>
                        <td><input class="botao2" type="submit" value="Alterar">
    |]

postEditarQtdePedidoR :: PedidomolduraId -> Handler Html
postEditarQtdePedidoR pedmolid = do
            (pedidomolduraStatus, pedidomolduraObservacao) <- runInputPost $ (,)
                <$> ireq textField "status"
                <*> ireq textField "pedidomolduraObservacao"
            _ <- runDB $ update pedmolid [PedidomolduraStatus =. pedidomolduraStatus, PedidomolduraObservacao =. pedidomolduraObservacao]
            redirect ListPedidomolduraR

getListPedidomolduraR :: Handler Html
getListPedidomolduraR = do
            molduras <- runDB $ selectList [] [Asc PedidomolduraStatus]
            defaultLayout $(whamletFile "templates/tabela7.hamlet")

postDelPedidomolduraR :: PedidomolduraId -> Handler Html
postDelPedidomolduraR pedmolid = do 
                runDB $ delete pedmolid
                redirect ListPedidomolduraR

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