{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Orcmoldura where

import Import
import Database.Persist.Postgresql

formOrcmoldura :: Form Orcmoldura
formOrcmoldura = renderDivs $ Orcmoldura
    <$> areq (selectFieldList modelo) "Modelo escolhido:"          Nothing
    <*> areq (selectFieldList lado1)  "Tamanho do LADO 1 *:  "       Nothing
    <*> areq (selectFieldList lado2)  "Tamanho do LADO 2 *:  "       Nothing
    <*> areq (selectFieldList paspatour) "Paspatour (parte branca entre o quadro e a moldura):  "              Nothing

formOrcmoldura1 :: Form Orcmoldura1
formOrcmoldura1 = renderDivs $ Orcmoldura1
    <$> areq (selectFieldList modelo) "Modelo escolhido:  "           Nothing
    <*> areq doubleField  "Tamanho do LADO 1 *:  "                    Nothing
    <*> areq doubleField  "Tamanho do LADO 2 *:  "                    Nothing
    <*> areq (selectFieldList paspatour) "Paspatour (parte branca entre o quadro e a moldura):  "               Nothing

lado1 :: [(Text, Double)]
lado1 = [("10 centímetros", 0.10), ("11 centímetros", 0.11), ("12 centímetros", 0.12), ("13 centímetros", 0.13), ("14 centímetros", 0.14), ("15 centímetros", 0.15), ("16 centímetros", 0.16), ("17 centímetros", 0.17), ("18 centímetros", 0.18), ("19 centímetros", 0.19), ("20 centímetros", 0.20), ("21 centímetros", 0.21), ("22 centímetros", 0.22), ("23 centímetros", 0.23), ("24 centímetros", 0.24), ("25 centímetros", 0.25), ("26 centímetros", 0.26), ("27 centímetros", 0.27), ("28 centímetros", 0.28), ("29 centímetros", 0.29), ("30 centímetros", 0.30), ("31 centímetros", 0.31), ("32 centímetros", 0.32), ("33 centímetros", 0.33), ("34 centímetros", 0.34), ("35 centímetros", 0.35), ("36 centímetros", 0.36), ("37 centímetros", 0.37), ("38 centímetros", 0.38), ("39 centímetros", 0.39), ("40 centímetros", 0.40), ("41 centímetros", 0.41), ("42 centímetros", 0.42), ("43 centímetros", 0.43), ("44 centímetros", 0.44), ("45 centímetros", 0.45), ("46 centímetros", 0.46), ("47 centímetros", 0.47), ("48 centímetros", 0.48), ("49 centímetros", 0.49), ("50 centímetros", 0.50), ("51 centímetros", 0.51), ("52 centímetros", 0.52), ("53 centímetros", 0.53), ("54 centímetros", 0.54), ("55 centímetros", 0.55), ("56 centímetros", 0.56), ("57 centímetros", 0.57), ("58 centímetros", 0.58), ("59 centímetros", 0.59), ("60 centímetros", 0.60), ("61 centímetros", 0.61), ("62 centímetros", 0.62), ("63 centímetros", 0.63), ("64 centímetros", 0.64), ("65 centímetros", 0.65), ("66 centímetros", 0.66), ("67 centímetros", 0.67), ("68 centímetros", 0.68), ("69 centímetros", 0.69), ("70 centímetros", 0.70), ("71 centímetros", 0.71), ("72 centímetros", 0.72), ("73 centímetros", 0.73), ("74 centímetros", 0.74), ("75 centímetros", 0.75), ("76 centímetros", 0.76), ("77 centímetros", 0.77), ("78 centímetros", 0.78), ("79 centímetros", 0.79), ("80 centímetros", 0.80), ("81 centímetros", 0.81), ("82 centímetros", 0.82), ("83 centímetros", 0.83), ("84 centímetros", 0.84), ("85 centímetros", 0.85), ("86 centímetros", 0.86), ("87 centímetros", 0.87), ("88 centímetros", 0.88), ("89 centímetros", 0.89), ("90 centímetros", 0.90), ("91 centímetros", 0.91), ("92 centímetros", 0.92), ("93 centímetros", 0.93), ("94 centímetros", 0.94), ("95 centímetros", 0.95), ("96 centímetros", 0.96), ("97 centímetros", 0.97), ("98 centímetros", 0.98), ("99 centímetros", 0.99), ("1 metro", 1)]

lado2 :: [(Text, Double)]
lado2 = [("10 centímetros", 0.10), ("11 centímetros", 0.11), ("12 centímetros", 0.12), ("13 centímetros", 0.13), ("14 centímetros", 0.14), ("15 centímetros", 0.15), ("16 centímetros", 0.16), ("17 centímetros", 0.17), ("18 centímetros", 0.18), ("19 centímetros", 0.19), ("20 centímetros", 0.20), ("21 centímetros", 0.21), ("22 centímetros", 0.22), ("23 centímetros", 0.23), ("24 centímetros", 0.24), ("25 centímetros", 0.25), ("26 centímetros", 0.26), ("27 centímetros", 0.27), ("28 centímetros", 0.28), ("29 centímetros", 0.29), ("30 centímetros", 0.30), ("31 centímetros", 0.31), ("32 centímetros", 0.32), ("33 centímetros", 0.33), ("34 centímetros", 0.34), ("35 centímetros", 0.35), ("36 centímetros", 0.36), ("37 centímetros", 0.37), ("38 centímetros", 0.38), ("39 centímetros", 0.39), ("40 centímetros", 0.40), ("41 centímetros", 0.41), ("42 centímetros", 0.42), ("43 centímetros", 0.43), ("44 centímetros", 0.44), ("45 centímetros", 0.45), ("46 centímetros", 0.46), ("47 centímetros", 0.47), ("48 centímetros", 0.48), ("49 centímetros", 0.49), ("50 centímetros", 0.50), ("51 centímetros", 0.51), ("52 centímetros", 0.52), ("53 centímetros", 0.53), ("54 centímetros", 0.54), ("55 centímetros", 0.55), ("56 centímetros", 0.56), ("57 centímetros", 0.57), ("58 centímetros", 0.58), ("59 centímetros", 0.59), ("60 centímetros", 0.60), ("61 centímetros", 0.61), ("62 centímetros", 0.62), ("63 centímetros", 0.63), ("64 centímetros", 0.64), ("65 centímetros", 0.65), ("66 centímetros", 0.66), ("67 centímetros", 0.67), ("68 centímetros", 0.68), ("69 centímetros", 0.69), ("70 centímetros", 0.70), ("71 centímetros", 0.71), ("72 centímetros", 0.72), ("73 centímetros", 0.73), ("74 centímetros", 0.74), ("75 centímetros", 0.75), ("76 centímetros", 0.76), ("77 centímetros", 0.77), ("78 centímetros", 0.78), ("79 centímetros", 0.79), ("80 centímetros", 0.80), ("81 centímetros", 0.81), ("82 centímetros", 0.82), ("83 centímetros", 0.83), ("84 centímetros", 0.84), ("85 centímetros", 0.85), ("86 centímetros", 0.86), ("87 centímetros", 0.87), ("88 centímetros", 0.88), ("89 centímetros", 0.89), ("90 centímetros", 0.90), ("91 centímetros", 0.91), ("92 centímetros", 0.92), ("93 centímetros", 0.93), ("94 centímetros", 0.94), ("95 centímetros", 0.95), ("96 centímetros", 0.96), ("97 centímetros", 0.97), ("98 centímetros", 0.98), ("99 centímetros", 0.99), ("1 metro", 1)]

modelo :: [(Text, Double)]
modelo = [("Veja a lista de molduras no botão acima", 0),("004", 25), ("024", 25), ("028", 25), ("032", 30), ("035", 25), ("043", 25), ("275", 35), ("320", 35), ("401", 35), ("416", 35), ("418", 35), ("419", 35), ("426", 40), ("428", 25), ("432", 30), ("438", 50), ("440", 45), ("511", 30), ("523", 35), ("712", 50), ("724", 45), ("765", 35), ("766", 40), ("767", 40), ("769", 40), ("775", 40), ("794", 35), ("802", 35), ("810", 40)]

paspatour :: [(Text, Double)]
paspatour = [("Não", 0), ("Sim", 30)]

getOrcmolduraR :: Handler Html
getOrcmolduraR = do
            (widget, enctype) <- generateFormPost formOrcmoldura
            defaultLayout $ do
                setTitle "Cidinha Porcelanas"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
                addStylesheet $ StaticR css_estilo_css
                toWidgetHead [hamlet|
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                <base href="https://haskmu-romefeller.c9users.io/">
                |]
                $(whamletFile "templates/menu2.hamlet")
                [whamlet|
                    <div class="orcamento1"><h2>Orçamento de Molduras</h2>
                        <p class="center">Para fazer o seu orçamento você precisa medir o seu quadro, ou gravura.</p>
                        <p class="center"><a href="@{ModelosMoldurasR}" class="botao" class="link">VEJA AQUI MODELOS DE MOLDURAS DISPONÍVEIS</a></p>
                        <form method=post action=@{OrcmolduraR} enctype=#{enctype}>
                            ^{widget}
                            <input class="botao2" type="submit" value="Fazer Orçamento"> <br>
                            <p><a href="@{Orcmoldura1R}" class="negrito">Clique aqui para fazer orçamento de quadros com medidas maiores que 1 metro</a>
                            <p><span class="negrito">* OBSERVAÇÃO:  </span>- Verifique se as medidas do seu quadro se encaixam na forma explicada no topo desta página;<br>
                                - Paspatour é a parte que fica entre a moldura e a gravura, normalmente nas cores branca e bege.</div>
                |]
                $(whamletFile "templates/footer.hamlet")

getOrcmoldura1R :: Handler Html
getOrcmoldura1R = do
            (widget, enctype) <- generateFormPost formOrcmoldura1
            defaultLayout $ do
                setTitle "Cidinha Porcelanas"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
                addStylesheet $ StaticR css_estilo_css
                toWidgetHead [hamlet|
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                <base href="https://haskmu-romefeller.c9users.io/">
                |]
                $(whamletFile "templates/menu2.hamlet")
                [whamlet|
                    <div class="orcamento1"><h2>Orçamento de Molduras</h2>
                        <p>Para fazer o seu orçamento você precisa medir o seu quadro, ou gravura, e utilizar a escala de metros.</p><p> Por exemplo: Se o seu quadro tem 1 metro e 20 centímetros x e 1 metro e 30 centímetros como medidas, eles devem ser digitados da seguinte forma: Lado 1:  1,2 e Lado 2: 1,3.</p>
                        <p class="center"><a href="@{ModelosMoldurasR}" class="botao" class="link">VEJA AQUI MODELOS DE MOLDURAS DISPONÍVEIS</a></p>
                        <form method=post action=@{Orcmoldura1R} enctype=#{enctype}>
                            ^{widget}
                            <input class="botao2" type="submit" value="Fazer Orçamento"> <br>
                            <p><span class="negrito">* OBSERVAÇÃO:  </span>Verifique se as medidas do seu quadro se encaixam na forma explicada no topo desta página;<br></div>
                |]
                $(whamletFile "templates/footer.hamlet")

postOrcmolduraR :: Handler Html
postOrcmolduraR = do
            ((result, _), _) <- runFormPost formOrcmoldura
            case result of
                FormSuccess orcmoldura -> do
                    orcmolid <- runDB $ insert orcmoldura
                    redirect $ OrcmoldurafinalR orcmolid
                _ -> redirect HomeR

postOrcmoldura1R :: Handler Html
postOrcmoldura1R = do
            ((result, _), _) <- runFormPost formOrcmoldura1
            case result of
                FormSuccess orcmoldura1 -> do
                    orcmoid <- runDB $ insert orcmoldura1
                    redirect $ Orcmoldura1finalR orcmoid
                _ -> redirect HomeR

getOrcmoldurafinalR :: OrcmolduraId -> Handler Html
getOrcmoldurafinalR orcmolid = do
            molduras <- runDB $ selectList [OrcmolduraId ==. orcmolid] []
            defaultLayout [whamlet|
                ^{menu2}
                    $forall Entity orcmolid orcmoldura <- molduras
                        <div class="orcamento1"><h2>Orçamento de número #{show $ fromSqlKey orcmolid}</h2>
                            <p> <span class="negrito">Medidas:</span> #{show (orcmolduraLado1 orcmoldura)} X #{show (orcmolduraLado2 orcmoldura)}
                            <p> <span class="negrito">Valor do quadro só com a Moldura: </span> R$ #{show (ceiling $ somoldura orcmoldura)}
                            <p> <span class="negrito">Valor do quadro com Moldura e Eucatex (madeira): </span> R$ #{show (ceiling $ molduraeuc orcmoldura)}
                            <p> <span class="negrito">Valor do quadro com Moldura e Vidro Antirreflexo: </span> R$ #{show (ceiling $ molduravar orcmoldura)}
                            <p> <span class="negrito">Valor do quadro com Moldura e Vidro Comum: </span> R$ #{show (ceiling $ molduravc orcmoldura)}
                            <p> <span class="negrito">Valor do quadro com Moldura e Espelho Simples: </span> R$ #{show (ceiling $ espelho orcmoldura)}<br><br>
                                <h3> OBSERVAÇÕES:</h3>
                                <p> - Caso você acredite que o valor do orçamento é maior que o esperado, por favor refaça o orçamento e preencha conforme descrição prévia;<br>
                                <p> - Os orçamentos realizados neste site são referentes a última tabela vingente, mas para ele ser concretizado deve ser levado o pedido, ou número do orçamento a loja física;<br>
                                <p> - Modelos e cores estão sujeitos a disponibilidade no ato do pedido;<br>
                                <p> - A confecção das molduras demora em média 7 dias úteis, conforme disponibilidade do material;<br>
                                <p> - Os valores podem sofrer alteração sem aviso prévio.<br><p><a href=@{OrcmolduraR} class="botao2">Fazer um novo orçamento</a><br><br><br></div>
                ^{rodape}
            |]
            where
                perimetro :: Orcmoldura -> Double
                perimetro orcmoldura = ((orcmolduraLado1 orcmoldura) + (orcmolduraLado2 orcmoldura)) * 2

                area :: Orcmoldura -> Double
                area orcmoldura = (orcmolduraLado1 orcmoldura) * (orcmolduraLado2 orcmoldura)

                somoldura :: Orcmoldura -> Double
                somoldura orcmoldura = ((orcmolduraModelo orcmoldura) + (orcmolduraPaspatour orcmoldura)) * (perimetro orcmoldura)

                molduraeuc :: Orcmoldura -> Double
                molduraeuc orcmoldura = (((orcmolduraModelo orcmoldura) + (orcmolduraPaspatour orcmoldura)) * (perimetro orcmoldura)) + (area orcmoldura * 40)

                molduravar :: Orcmoldura -> Double
                molduravar orcmoldura = ((perimetro orcmoldura) * ((orcmolduraModelo orcmoldura) + (orcmolduraPaspatour orcmoldura))) + ((area orcmoldura) * 200)
    
                molduravc :: Orcmoldura -> Double
                molduravc orcmoldura = ((perimetro orcmoldura) * ((orcmolduraModelo orcmoldura) + (orcmolduraPaspatour orcmoldura))) + ((area orcmoldura) * 160)

                espelho :: Orcmoldura -> Double
                espelho orcmoldura = ((perimetro orcmoldura) * ((orcmolduraModelo orcmoldura) + (orcmolduraPaspatour orcmoldura))) + ((area orcmoldura) * 400)

getOrcmoldura1finalR :: Orcmoldura1Id -> Handler Html
getOrcmoldura1finalR orcmoid = do
            moldurass <- runDB $ selectList [Orcmoldura1Id ==. orcmoid] []
            defaultLayout [whamlet|
                ^{menu2}
                    $forall Entity orcmoid orcmoldura1 <- moldurass
                        <div class="orcamento1"><h2>Orçamento de número #{show $ fromSqlKey orcmoid}</h2>
                            <p> <span class="negrito">Medidas:</span> #{show (orcmoldura1Lado1 orcmoldura1)} X #{show (orcmoldura1Lado2 orcmoldura1)}
                            <p> <span class="negrito">Valor do quadro só com a Moldura: </span> R$ #{show (ceiling $ somoldura orcmoldura1)}
                            <p> <span class="negrito">Valor do quadro com Moldura e Eucatex (madeira): </span> R$ #{show (ceiling $ molduraeuc orcmoldura1)}
                            <p> <span class="negrito">Valor do quadro com Moldura e Vidro Antirreflexo: </span> R$ #{show (ceiling $ molduravar orcmoldura1)}
                            <p> <span class="negrito">Valor do quadro com Moldura e Vidro Comum: </span> R$ #{show (ceiling $ molduravc orcmoldura1)}
                            <p> <span class="negrito">Valor do quadro com Moldura e Espelho Simples: </span> R$ #{show (ceiling $ espelho orcmoldura1)}<br><br>
                                <h3> OBSERVAÇÕES:</h3>
                                <p> - Caso você acredite que o valor do orçamento é maior que o esperado, por favor refaça o orçamento e preencha conforme descrição prévia;<br>
                                <p> - Os orçamentos realizados neste site são referentes a última tabela vingente, mas para ele ser concretizado deve ser levado o pedido, ou número do orçamento a loja física;<br>
                                <p> - Modelos e cores estão sujeitos a disponibilidade no ato do pedido;<br>
                                <p> - A confecção das molduras demora em média 7 dias úteis, conforme disponibilidade do material;<br>
                                <p> - Os valores podem sofrer alteração sem aviso prévio.<br><p><a href=@{Orcmoldura1R} class="botao2">Fazer um novo orçamento</a><br><br><br></div>
                ^{rodape}
            |]
            where
                perimetro :: Orcmoldura1 -> Double
                perimetro orcmoldura1 = ((orcmoldura1Lado1 orcmoldura1) + (orcmoldura1Lado2 orcmoldura1)) * 2

                area :: Orcmoldura1 -> Double
                area orcmoldura1 = (orcmoldura1Lado1 orcmoldura1) * (orcmoldura1Lado2 orcmoldura1)

                somoldura :: Orcmoldura1 -> Double
                somoldura orcmoldura1 = ((orcmoldura1Modelo orcmoldura1) + (orcmoldura1Paspatour orcmoldura1)) * (perimetro orcmoldura1)

                molduraeuc :: Orcmoldura1 -> Double
                molduraeuc orcmoldura1 = (((orcmoldura1Modelo orcmoldura1) + (orcmoldura1Paspatour orcmoldura1)) * (perimetro orcmoldura1)) + (area orcmoldura1 * 40)

                molduravar :: Orcmoldura1 -> Double
                molduravar orcmoldura1 = ((perimetro orcmoldura1) * ((orcmoldura1Modelo orcmoldura1) + (orcmoldura1Paspatour orcmoldura1))) + ((area orcmoldura1) * 200)
    
                molduravc :: Orcmoldura1 -> Double
                molduravc orcmoldura1 = ((perimetro orcmoldura1) * ((orcmoldura1Modelo orcmoldura1) + (orcmoldura1Paspatour orcmoldura1))) + ((area orcmoldura1) * 160)

                espelho :: Orcmoldura1 -> Double
                espelho orcmoldura1 = ((perimetro orcmoldura1) * ((orcmoldura1Modelo orcmoldura1) + (orcmoldura1Paspatour orcmoldura1))) + ((area orcmoldura1) * 400)

getListOrcmolduraR :: Handler Html
getListOrcmolduraR = do
            molduras <- runDB $ selectList [] [Desc OrcmolduraId]
            defaultLayout $(whamletFile "templates/tabela5.hamlet")

getListOrcmoldura1R :: Handler Html
getListOrcmoldura1R = do
            moldurass <- runDB $ selectList [] [Desc Orcmoldura1Id]
            defaultLayout $(whamletFile "templates/tabela10.hamlet")

postDelOrcmolduraR :: OrcmolduraId -> Handler Html
postDelOrcmolduraR orcmolid = do 
                runDB $ delete orcmolid
                redirect ListOrcmolduraR

postDelOrcmoldura1R :: Orcmoldura1Id -> Handler Html
postDelOrcmoldura1R orcmoid = do 
                runDB $ delete orcmoid
                redirect ListOrcmoldura1R

menu2 :: Widget
menu2 = do
    setTitle "Cidinha Porcelanas"
    addStylesheetRemote "https://fonts.googleapis.com/css?family=Roboto:300"
    addStylesheet $ StaticR css_estilo_css
    toWidgetHead [hamlet|
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <base href="https://haskmu-romefeller.c9users.io/">
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
