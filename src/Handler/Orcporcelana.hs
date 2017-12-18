{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Orcporcelana where

import Import
import Database.Persist.Postgresql


formOrcporcelana :: Form Orcporcelana
formOrcporcelana = renderDivs $ Orcporcelana
    <$> areq (selectFieldList loucas) "Escolha a louça:  "          Nothing
    <*> areq doubleField "Quantidade:  "                            (Just 0)
    <*> areq (selectFieldList arte) "Arte a ser desenvolvida:  "    Nothing
 
formOrcporcelana1 :: Form Orcporcelana1
formOrcporcelana1 = renderDivs $ Orcporcelana1
    <$> areq (selectFieldList loucas) "Escolha a primeira louça:  " Nothing
    <*> areq doubleField  "Quantidade da primeira louça:  "         (Just 0)
    <*> areq (selectFieldList arte) "Arte a ser desenvolvida para a primeira louça:  " Nothing
    <*> areq (selectFieldList loucas) "Escolha a segunda louça:  "  Nothing
    <*> areq doubleField  "Quantidade da segunda louça:  "          (Just 0)
    <*> areq (selectFieldList arte) "Arte a ser desenvolvida na segunda louça:  " Nothing
    <*> areq (selectFieldList loucas) "Escolha a terceira louça:  " Nothing
    <*> areq doubleField  "Quantidade da terceira louça:  "       (Just 0)
    <*> areq (selectFieldList arte) "Arte a ser desenvolvida na terceira louça:  " Nothing

loucas :: [(Text, Double)]
loucas = [("NENHUM", 0), ("Boll - Pequeno", 11.90), ("Boll - Médio", 14.90), ("Boll - Grande", 16.90),  ("Bule (com tampa) - Médio", 35.50),  ("Bule (com tampa) - Grande", 55.50), ("Caneca - Pequena", 6.00), ("Caneca - Média", 9.90), ("Caneca - Grande", 11.90), ("Cinzeiro", 18.95), ("Consumê (com pires)", 22.00), ("Copo de Vidro - Pequeno (shot)", 10.00), ("Copo de Vidro - Médio", 12.50), ("Copo de Vidro - Grande", 15.00), ("Leiteira", 37.50), ("Moringa - Pequena", 24.00), ("Moringa - Média", 29.00), ("Moringa - Grande", 39.90), ("Porta Guardanapo", 15.00), ("Porta Temperos", 6.00), ("Potes (com tampa) - Pequeno", 36.50), ("Potes (com tampa) - Médio", 49.50), ("Potes (com tampa) - Grande", 55.00), ("Prato de Bolo REDONDO", 54.00), ("Prato de Pão REDONDO", 12.00), ("Prato de Sobremesa REDONDO", 18.00), ("Prato Fundo REDONDO", 20.00), ("Prato Raso REDONDO (tradicional)", 22.00), ("Prato de Bolo QUADRADO", 55.00), ("Prato de Pão QUADRADO", 17.00), ("Prato de Sobremesa QUADRADO", 22.00), ("Prato Fundo QUADRADO", 28.00), ("Prato Raso QUADRADO (tradicional)", 30.00), ("Taça de vidro", 15.00), ("Xícara (com pires) - Pequena", 10.50), ("Xícara (com pires) - Média", 14.80), ("Xícara (com pires) - Grande", 16.50)]

arte :: [(Text, Double)]
arte = [("NENHUMA", 0), ("Pintura a mão", 50.00), ("Decalque do acervo", 15.00), ("Decalque Personalizado (valor de no mínimo de 50 decalques)", 200.00)]

getOrcporcelanaR :: Handler Html
getOrcporcelanaR = do
            (widget, enctype) <- generateFormPost formOrcporcelana
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
                    <div class="orcamento1"><h2>Orçamento de Porcelanas</h2><form>
                        ^{widget}
                        <input class="botao2" enctype=#{enctype} type="submit" formmethod="post" formaction=@{OrcporcelanaR} value="Fazer orçamento"></form>
                        <form><input type="submit" formmethod="get" value="Fazer orçamento de várias peças" formaction=@{Orcporcelana1R} class="botao2"></form><br>
                            <h3> OBSERVAÇÕES:</h3>
                            <p> - Decalque é uma arte impressa, que pode ser encontrada em diversos desenhos e estilos, e pode ser feita personalizada para uma empresa ou pessoa;<br>
                            <p> - Caso você esteja fazendo um orçamento com decalque personalizado, não é necessário escolher as artes de um mesmo orçamento;<br>
                    ^{rodape}
                |]

getOrcporcelana1R :: Handler Html
getOrcporcelana1R = do
            (widget, enctype) <- generateFormPost formOrcporcelana1
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
                    <div class="orcamento1"><h2>Orçamento de Porcelanas</h2><form method=post action=@{Orcporcelana1R} enctype=#{enctype}>
                        ^{widget}
                        <input class="botao2" type="submit" value="Fazer orçamento"></div>
                |]
                $(whamletFile "templates/footer.hamlet")

postOrcporcelanaR :: Handler Html
postOrcporcelanaR = do
            ((result, _), _) <- runFormPost formOrcporcelana
            case result of
                FormSuccess orcporcelana -> do
                    orcporcid <- runDB $ insert orcporcelana
                    redirect $ OrcporcelanafinalR orcporcid
                _ -> redirect HomeR

postOrcporcelana1R :: Handler Html
postOrcporcelana1R = do
            ((result, _), _) <- runFormPost formOrcporcelana1
            case result of
                FormSuccess orcporcelana1 -> do
                    orcporid <- runDB $ insert orcporcelana1
                    redirect $ Orcporcelana1finalR orcporid
                _ -> redirect HomeR

getOrcporcelanafinalR :: OrcporcelanaId -> Handler Html
getOrcporcelanafinalR orcporcid = do
            porcelanas <- runDB $ selectList [OrcporcelanaId ==. orcporcid] []
            defaultLayout [whamlet|
                ^{menu2}
                    $forall Entity orcporcid orcporcelana <- porcelanas
                        <div class="orcamento1"><h2>Orçamento de número #{show $ fromSqlKey orcporcid}</h2>
                            <p> <span class="negrito">Quantidade de Louças escolhidas:</span> #{show (orcporcelanaQtde orcporcelana)} unidades.
                            <p> <span class="negrito">Valor total: </span> R$ #{show (ceiling $ porcelanavalor orcporcelana)}<br><br>
                                <h3> OBSERVAÇÕES:</h3>
                                <p> - Os orçamentos realizados neste site são referentes a última tabela vingente, mas para ele ser concretizado deve ser levado o pedido, ou número do orçamento a loja física;<br>
                                <p> - Modelos e cores estão sujeitos a disponibilidade no ato do pedido;<br>
                                <p> - A confecção das louças demora em média 5 dias úteis, conforme disponibilidade do material;<br>
                                <p> - Decalques personalizados tem um pedido mínimo de 50 artes para ser produzido, indepedente da quantidade de louças produzidas seu valor é fixo;<br>
                                <p> - Outras louças como vasos, porta jóias, miniaturas, entre outras, somente são comercializadas através de encomenda;<br>
                                <p> - Os valores podem sofrer alteração sem aviso prévio.<br><p><a href=@{OrcporcelanaR} class="botao2">Fazer um novo orçamento</a><br><br><br></div>
                ^{rodape}
            |]
            where
                porcelanavalor :: Orcporcelana -> Double
                porcelanavalor orcporcelana = (((orcporcelanaLouca orcporcelana) * (orcporcelanaQtde orcporcelana)) + (orcporcelanaArte orcporcelana))

getOrcporcelana1finalR :: Orcporcelana1Id -> Handler Html
getOrcporcelana1finalR orcporid = do
            porcelanass <- runDB $ selectList [Orcporcelana1Id ==. orcporid] []
            defaultLayout [whamlet|
                ^{menu2}
                    $forall Entity orcporid orcporcelana1 <- porcelanass
                        <div class="orcamento1"><h2>Orçamento de número #{show $ fromSqlKey orcporid}</h2>
                            <p> <span class="negrito">Quantidade de Louças escolhidas:</span> #{show (qtdetotal1 orcporcelana1)} unidades.
                            <p> <span class="negrito">Valor total: </span> R$ #{show (ceiling $ porcelanavalor1 orcporcelana1)}<br><br>
                                <h3> OBSERVAÇÕES:</h3>
                                <p> - Os orçamentos realizados neste site são referentes a última tabela vingente, mas para ele ser concretizado deve ser levado o pedido, ou número do orçamento a loja física;<br>
                                <p> - Modelos e cores estão sujeitos a disponibilidade no ato do pedido;<br>
                                <p> - A confecção das louças demora em média 5 dias úteis, conforme disponibilidade do material;<br>
                                <p> - Decalques personalizados tem um pedido mínimo de 50 artes para ser produzido, indepedente da quantidade de louças produzidas seu valor é fixo;<br>
                                <p> - Caso você esteja fazendo um orçamento com decalque personalizado, não é necessário escolher as artes de um mesmo orçamento;<br>
                                <p> - Outras louças como vasos, porta jóias, miniaturas, entre outras, somente são comercializadas através de encomenda;<br>
                                <p> - Os valores podem sofrer alteração sem aviso prévio.<br><p><a href=@{OrcporcelanaR} class="botao2">Fazer um novo orçamento</a><br><br><br></div>
                ^{rodape}
            |]
            where
                porcelanavalor1 :: Orcporcelana1 -> Double
                porcelanavalor1 orcporcelana1 = (((orcporcelana1Louca1 orcporcelana1) * (orcporcelana1Qtde1 orcporcelana1)) + (orcporcelana1Arte1 orcporcelana1)) + (((orcporcelana1Louca2 orcporcelana1) * (orcporcelana1Qtde2 orcporcelana1)) + (orcporcelana1Arte2 orcporcelana1)) + (((orcporcelana1Louca3 orcporcelana1) * (orcporcelana1Qtde3 orcporcelana1)) + (orcporcelana1Arte3 orcporcelana1))

                qtdetotal1 :: Orcporcelana1 -> Double
                qtdetotal1 orcporcelana1 = (orcporcelana1Qtde1 orcporcelana1) + (orcporcelana1Qtde2 orcporcelana1) + (orcporcelana1Qtde3 orcporcelana1)

getListOrcporcelanaR :: Handler Html
getListOrcporcelanaR = do
            porcelanas <- runDB $ selectList [] [Desc OrcporcelanaId]
            defaultLayout $(whamletFile "templates/tabela4.hamlet")

getListOrcporcelana1R :: Handler Html
getListOrcporcelana1R = do
            porcelanass <- runDB $ selectList [] [Desc Orcporcelana1Id]
            defaultLayout $(whamletFile "templates/tabela9.hamlet")
                
postDelOrcporcelanaR :: OrcporcelanaId -> Handler Html
postDelOrcporcelanaR orcporcid = do 
                runDB $ delete orcporcid
                redirect ListOrcporcelanaR

postDelOrcporcelana1R :: Orcporcelana1Id -> Handler Html
postDelOrcporcelana1R orcporid = do 
                runDB $ delete orcporid
                redirect ListOrcporcelana1R

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
