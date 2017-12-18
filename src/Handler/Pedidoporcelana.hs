{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Pedidoporcelana where

import Import
import Database.Persist.Postgresql


formPedidoporcelana :: Form Pedidoporcelana
formPedidoporcelana = renderDivs $ Pedidoporcelana
    <$> areq (selectFieldList status)  "Status: "            Nothing
    <*> areq textField  "Data de Entrada: "                  Nothing
    <*> areq textField  "Nome do Cliente: "                  Nothing
    <*> areq textField  "Telefone: "                         Nothing
    <*> areq textField  "Tipo: "                             Nothing
    <*> areq textField  "Sub-tipo: "                         Nothing
    <*> areq intField  "Quantidade: "                        Nothing
    <*> areq textField  "Arte: "                             Nothing
    <*> areq textField  "Observação: "                       Nothing
    <*> areq textField  "Forma de Pagamento: "               Nothing
    <*> areq doubleField  "Valor Final: "                    Nothing

status :: [(Text, Text)]
status = [("Em Andamento", "Em Andamento"), ("Finalizado", "Finalizado")]

getPedidoporcelanaR :: Handler Html
getPedidoporcelanaR = do
            (widget, enctype) <- generateFormPost formPedidoporcelana
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
                     <h2 align="center"> Cadastrar Pedido porcelanas
                     <form method=post action=@{PedidoporcelanaR} enctype=#{enctype}>
                         ^{widget}
                         <input type="submit" value="Cadastrar">
                |]
                

postPedidoporcelanaR :: Handler Html
postPedidoporcelanaR = do
            ((result, _), _) <- runFormPost formPedidoporcelana
            case result of
                FormSuccess pedidoporcelana -> do
                    pedporid <- runDB $ insert pedidoporcelana
                    redirect ListPedidoporcelanaR
                _ -> redirect HomeR

getListPedidoporcelanaR :: Handler Html
getListPedidoporcelanaR = do
            porcelanas <- runDB $ selectList [] [Asc PedidoporcelanaStatus]
            defaultLayout $(whamletFile "templates/tabela8.hamlet")

postDelPedidoporcelanaR :: PedidoporcelanaId -> Handler Html
postDelPedidoporcelanaR pedporid = do 
                runDB $ delete pedporid
                redirect ListPedidoporcelanaR

getEditarQuantidadePedidoR :: PedidoporcelanaId -> Handler Html
getEditarQuantidadePedidoR pedporid = do
    porcelanas <- runDB $ selectList [PedidoporcelanaId ==. pedporid] []
    defaultLayout [whamlet|
        ^{menu3}
        $forall Entity pedporid pedidoporcelana <- porcelanas
            <h2 align="center">Editar Pedido de porcelanas</h2>
                <tr>
                    <form action=@{EditarQuantidadePedidoR pedporid} method=post>
                        <td><span class="negrito">Status:</span> <select name="status"><option value="Em Andamento">Em Andamento</option><option value="Finalizado">Finalizado</option></select><br>
                        <td><span class="negrito">Entrada:</span> #{unpack (pedidoporcelanaEntrada  pedidoporcelana)}<br>
                        <td><span class="negrito">Nome Cliente:</span>  #{unpack (pedidoporcelanaClientenome     pedidoporcelana)}<br>
                        <td><span class="negrito">Telefone:</span> #{unpack (pedidoporcelanaClientetelefone pedidoporcelana)}<br>
                        <td><span class="negrito">Tipo:</span> #{unpack (pedidoporcelanaTipo  pedidoporcelana)}<br>
                        <td><span class="negrito">Sub-tipo:</span> #{unpack (pedidoporcelanaSubtipo  pedidoporcelana)}<br>
                        <td><span class="negrito">Quantidade:</span> #{show (pedidoporcelanaQuantidade  pedidoporcelana)}<br>
                        <td><span class="negrito">Arte:</span> #{unpack (pedidoporcelanaArte  pedidoporcelana)}<br>
                        <td><span class="negrito">Observação:</span> #{unpack (pedidoporcelanaObservacao  pedidoporcelana)}<br>
                        <td><span class="negrito">Forma de Pagamento:</span> <input type="text" name="pedidoporcelanaPagamento" value=#{unpack (pedidoporcelanaPagamento pedidoporcelana)}><br>
                        <td><span class="negrito">Valor Final:</span> #{show (pedidoporcelanaValorfinal  pedidoporcelana)}<br>
                        <td><input class="botao2" type="submit" value="Alterar">
    |]
    
postEditarQuantidadePedidoR :: PedidoporcelanaId -> Handler Html
postEditarQuantidadePedidoR pedporid = do
            (pedidoporcelanaStatus, pedidoporcelanaPagamento) <- runInputPost $ (,)
                <$> ireq textField  "status"
                <*> ireq textField  "pedidoporcelanaPagamento"
            _ <- runDB $ update pedporid [PedidoporcelanaStatus =. pedidoporcelanaStatus, PedidoporcelanaPagamento =. pedidoporcelanaPagamento]
            redirect ListPedidoporcelanaR

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
