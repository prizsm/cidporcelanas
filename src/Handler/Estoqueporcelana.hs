{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Estoqueporcelana where

import Import
import Database.Persist.Postgresql


formEstoqueporcelana :: Form Estoqueporcelana
formEstoqueporcelana = renderDivs $ Estoqueporcelana
    <$> areq intField  "Código:  "       Nothing
    <*> areq textField "Tipo:  "         Nothing
    <*> areq textField "Sub-Tipo:  "     Nothing
    <*> areq textField "Tamanho:  "      Nothing
    <*> areq intField  "Quantidade:  "   Nothing
    <*> areq textField "Observação:  "   Nothing

getEstoqueporcelanaR :: Handler Html
getEstoqueporcelanaR = do
            (widget, enctype) <- generateFormPost formEstoqueporcelana
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
                     <h2 align="center"> Cadastrar Estoque Porcelanas
                     <form method=post action=@{EstoqueporcelanaR} enctype=#{enctype}>
                         ^{widget}
                         <input type="submit" class="botao2" value="Cadastrar">
                |]
                

postEstoqueporcelanaR :: Handler Html
postEstoqueporcelanaR = do
            ((result, _), _) <- runFormPost formEstoqueporcelana
            case result of
                FormSuccess estoqueporcelana -> do
                    porcid <- runDB $ insert estoqueporcelana
                    redirect ListEstoqueporcelanaR
                _ -> redirect HomeR

getListEstoqueporcelanaR :: Handler Html
getListEstoqueporcelanaR = do
            porcelanas <- runDB $ selectList [] [Asc EstoqueporcelanaCodigo]
            defaultLayout $(whamletFile "templates/tabela3.hamlet")

postDelEstoqueporcelanaR :: EstoqueporcelanaId -> Handler Html
postDelEstoqueporcelanaR porcid = do 
                runDB $ delete porcid
                redirect ListEstoqueporcelanaR

getEditarQuantidadeEstoqueR :: EstoqueporcelanaId -> Handler Html
getEditarQuantidadeEstoqueR porcid = do
    porcelanas <- runDB $ selectList [EstoqueporcelanaId ==. porcid] []
    defaultLayout [whamlet|
        ^{menu3}
        $forall Entity porcid estoqueporcelana <- porcelanas
            <h2 align="center">Editar Estoque de Porcelanas</h2>
                <tr>
                    <form action=@{EditarQuantidadeEstoqueR porcid} method=post>
                        <td><span class="negrito">Código: </span>#{show (estoqueporcelanaCodigo estoqueporcelana)}<br>
                        <td><span class="negrito">Tipo: </span>#{unpack (estoqueporcelanaTipo estoqueporcelana)}<br>
                        <td><span class="negrito">Sub Tipo: </span> #{unpack (estoqueporcelanaSubtipo estoqueporcelana)}<br>
                        <td><span class="negrito">Tamanho:</span> #{unpack (estoqueporcelanaTamanho estoqueporcelana)}<br>
                        <td><span class="negrito">Quantidade: </span><input type="number" name="estoqueporcelanaQuantidade" value=#{show (estoqueporcelanaQuantidade estoqueporcelana)}><br>
                        <td><span class="negrito">Observação:</span> <input type="text" name="estoqueporcelanaObservacao" value=#{unpack (estoqueporcelanaObservacao estoqueporcelana)}><br>
                        <td><input class="botao2" type="submit" value="Alterar">
    |]
    
postEditarQuantidadeEstoqueR :: EstoqueporcelanaId -> Handler Html
postEditarQuantidadeEstoqueR porcid = do
            (estoqueporcelanaQuantidade, estoqueporcelanaObservacao) <- runInputPost $ (,)
                <$> ireq intField "estoqueporcelanaQuantidade"
                <*> ireq textField "estoqueporcelanaObservacao"
            _ <- runDB $ update porcid [EstoqueporcelanaQuantidade =. estoqueporcelanaQuantidade, EstoqueporcelanaObservacao =. estoqueporcelanaObservacao]
            redirect ListEstoqueporcelanaR

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
