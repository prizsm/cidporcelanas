{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Common where

import Import
import Database.Persist.Postgresql


formFalecom :: Form Falecom
formFalecom = renderDivs $ Falecom
    <$> areq textField "Nome:  "           Nothing
    <*> areq intField  "Telefone:  "       Nothing
    <*> areq textField "Email:  "          Nothing
    <*> areq textField "Mensagem:  "       Nothing

getAdmR :: Handler Html
getAdmR = do
    sess <- lookupSession "_ID"
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
            <h2 align="center"> ÁREA ADMINISTRATIVA
                $maybe _ <- sess
                    <div id="portfolio3">
                        <p class="negrito" class="titulo">CONTATOS:</p>
                        <p><a href="@{ListFalecomR}">Contatos recebidos</a></p>
                        <p class="negrito" class="titulo">USUÁRIOS:</p>
                        <p><a href="@{UsrR}">Cadastrar usuários novos</a></p>
                        <p><a href="@{ListUsrR}">Listar usuários cadastrados</a></p>
                        <p class="negrito" class="titulo">ESTOQUE:</p>                        
                        <p><a href="@{EstoqueporcelanaR}">Cadastrar estoque de porcelana</a></p>
                        <p><a href="@{ListEstoqueporcelanaR}">Listar estoque de porcelana</a></p>
                        <p><a href="@{EstoquemolduraR}">Cadastrar estoque de molduras</a></p>
                        <p><a href="@{ListEstoquemolduraR}">Listar estoque de molduras</a></p>
                        <p class="negrito" class="titulo">PEDIDOS:</p>
                        <p><a href="@{PedidomolduraR}">Cadastrar pedidos de molduras</a></p>
                        <p><a href="@{ListPedidomolduraR}">Listar pedidos de molduras</a></p>
                        <p><a href="@{PedidoporcelanaR}">Cadastrar pedidos de porcelanas</a></p>
                        <p><a href="@{ListPedidoporcelanaR}">Listar pedidos de porcelanas</a></p>
                        <p class="negrito" class="titulo">ORÇAMENTOS:</p>
                        <p><a href="@{ListOrcmolduraR}">Listar orçamento de molduras</a></p>
                        <p><a href="@{ListOrcmoldura1R}">Listar orçamento de molduras maiores de 1 metro</a></p>
                        <p><a href="@{ListOrcporcelanaR}">Listar orçamentos de porcelana única</a></p>
                        <p><a href="@{ListOrcporcelana1R}">Listar orçamentos de porcelanas várias peças</a></p>
                        <h3 class="center" class="titulo"><a href=@{LogoutR}>SAIR</a></p></h3>
                $nothing
                    <li> <a href=@{LoginR}>Login
        |]        
        $(whamletFile "templates/footer.hamlet")
        
getEnderecoR :: Handler Html
getEnderecoR = do
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu2.hamlet")
        $(whamletFile "templates/endereco.hamlet")
        $(whamletFile "templates/footer.hamlet")
        
getSobreR :: Handler Html
getSobreR = do
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu2.hamlet")
        $(whamletFile "templates/sobre.hamlet")
        $(whamletFile "templates/footer.hamlet")
        
getPortfolioR :: Handler Html
getPortfolioR = do
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu2.hamlet")
        $(whamletFile "templates/portfolio.hamlet")
        $(whamletFile "templates/footer.hamlet")
        
getModelosMoldurasR :: Handler Html
getModelosMoldurasR = do
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu2.hamlet")
        $(whamletFile "templates/modelosmolduras.hamlet")
        $(whamletFile "templates/footer.hamlet")
        
getCanecasR :: Handler Html
getCanecasR = do
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
            <div><a href="@{PortfolioR}" id="voltar">VOLTAR</a></div>
            <div id="portfolio2">
                <h2>CANECAS</h2>
                    <img src="static/img/portfolio/caneca001.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca002.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca003.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca004.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca005.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca006.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca007.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca008.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca009.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca010.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca011.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca012.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca013.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca014.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca015.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca016.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca017.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca018.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca019.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca020.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca021.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca022.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca023.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca024.jpg" class="img-port2">
                    <img src="static/img/portfolio/caneca025.jpg" class="img-port2">
        |]
        $(whamletFile "templates/footer.hamlet")
        
getXicarasR :: Handler Html
getXicarasR = do
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
            <div><a href="@{PortfolioR}" id="voltar">VOLTAR</a></div>
            <div id="portfolio2">
                <h2>XÍCARAS</h2>
                    <img src="static/img/portfolio/xicara001.jpg" class="img-port2">
                    <img src="static/img/portfolio/xicara002.jpg" class="img-port2">
                    <img src="static/img/portfolio/xicara003.jpg" class="img-port2">
                    <img src="static/img/portfolio/xicara004.jpg" class="img-port2">
                    <img src="static/img/portfolio/xicara005.jpg" class="img-port2">
                    <img src="static/img/portfolio/xicara006.jpg" class="img-port2">
        |]
        $(whamletFile "templates/footer.hamlet")
        
getPotesR :: Handler Html
getPotesR = do
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
            <div><a href="@{PortfolioR}" id="voltar">VOLTAR</a></div>
            <div id="portfolio2">
                <h2>POTES</h2>
                    <img src="static/img/portfolio/pote001.jpg" class="img-port2">
                    <img src="static/img/portfolio/pote002.jpg" class="img-port2">
                    <img src="static/img/portfolio/pote003.jpg" class="img-port2">
                    <img src="static/img/portfolio/pote004.jpg" class="img-port2">
                    <img src="static/img/portfolio/pote005.jpg" class="img-port2">
        |]
        $(whamletFile "templates/footer.hamlet")
        
getPratosR :: Handler Html
getPratosR = do
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
            <div><a href="@{PortfolioR}" id="voltar">VOLTAR</a></div>
            <div id="portfolio2">
                <h2>PRATOS</h2>
                    <img src="static/img/portfolio/prato001.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato002.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato003.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato004.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato005.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato006.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato007.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato008.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato009.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato010.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato011.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato012.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato013.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato014.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato015.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato016.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato017.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato018.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato019.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato020.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato021.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato022.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato023.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato024.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato025.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato026.jpg" class="img-port2">
                    <img src="static/img/portfolio/prato027.jpg" class="img-port2">
        |]
        $(whamletFile "templates/footer.hamlet")
        
getTacasR :: Handler Html
getTacasR = do
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
            <div><a href="@{PortfolioR}" id="voltar">VOLTAR</a></div>
            <div id="portfolio2">
                <h2>COPOS E TAÇAS</h2>
                    <img src="static/img/portfolio/taca001.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca002.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca003.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca004.jpg" class="img-port2">
                    <img src="static/img/portfolio/copo001.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca005.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca006.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca007.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca008.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca009.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca010.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca011.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca012.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca013.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca014.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca015.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca016.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca017.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca018.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca019.jpg" class="img-port2">
                    <img src="static/img/portfolio/taca020.jpg" class="img-port2">
        |]
        $(whamletFile "templates/footer.hamlet")
        
getVasosR :: Handler Html
getVasosR = do
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
            <div><a href="@{PortfolioR}" id="voltar">VOLTAR</a></div>
            <div id="portfolio2">
                <h2>VASOS</h2>
                    <img src="static/img/portfolio/vaso001.jpg" class="img-port2">
                    <img src="static/img/portfolio/vaso002.jpg" class="img-port2">
                    <img src="static/img/portfolio/vaso003.jpg" class="img-port2">
                    <img src="static/img/portfolio/vaso004.jpg" class="img-port2">
                    <img src="static/img/portfolio/vaso005.jpg" class="img-port2">
                    <img src="static/img/portfolio/vaso006.jpg" class="img-port2">
                    <img src="static/img/portfolio/vaso007.jpg" class="img-port2">
                    <img src="static/img/portfolio/vaso008.jpg" class="img-port2">
                    <img src="static/img/portfolio/vaso009.jpg" class="img-port2">
                    <img src="static/img/portfolio/vaso010.jpg" class="img-port2">
        |]
        $(whamletFile "templates/footer.hamlet")
        
getQuadroR :: Handler Html
getQuadroR = do
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
            <div><a href="@{PortfolioR}" id="voltar">VOLTAR</a></div>
            <div id="portfolio2">
                <h2>QUADROS</h2>
                    <img src="static/img/portfolio/quadro001.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro002.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro003.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro004.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro005.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro006.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro007.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro008.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro009.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro010.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro011.jpg" class="img-port2">
                    <img src="static/img/portfolio/quadro012.jpg" class="img-port2">
        |]
        $(whamletFile "templates/footer.hamlet")
        
getPublicacoesR :: Handler Html
getPublicacoesR = do
    defaultLayout $ do
        setTitle "Cidinha Porcelanas"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
        addStylesheet $ StaticR css_estilo_css
        toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        |]
        $(whamletFile "templates/menu2.hamlet")
        $(whamletFile "templates/publicacoes.hamlet")
        $(whamletFile "templates/footer.hamlet")
        
getOutrosR :: Handler Html
getOutrosR = do
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
            <div><a href="@{PortfolioR}" id="voltar">VOLTAR</a></div>
            <div id="portfolio2">
                <h2>OUTROS</h2>
                    <img src="static/img/portfolio/relogio001.jpg" class="img-port2">
                    <img src="static/img/portfolio/relogio002.jpg" class="img-port2">
                    <img src="static/img/portfolio/relogio003.jpg" class="img-port2">
                    <img src="static/img/portfolio/cinzeiro001.jpg" class="img-port2">
                    <img src="static/img/portfolio/cinzeiro002.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros001.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros002.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros003.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros004.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros005.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros006.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros007.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros008.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros009.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros010.jpg" class="img-port2">
                    <img src="static/img/portfolio/outros011.jpg" class="img-port2">
        |]
        $(whamletFile "templates/footer.hamlet")
        