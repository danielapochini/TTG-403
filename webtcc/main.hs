{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             TupleSections, OverloadedStrings,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Yesod.Static
import Yesod.Form.Bootstrap3
import Control.Monad.Logger (runStdoutLoggingT)



data SauipeExpress = SauipeExpress {getStatic :: Static}

instance Yesod SauipeExpress

staticFiles "static"

mkYesod "SauipeExpress" [parseRoutes|
/ HomeR GET 
/quemsomos QuemSomosR GET
/servicos ServicosR GET 
/contato ContatoR GET 
/static StaticR Static getStatic
|]

getContatoR :: Handler Html
getContatoR = defaultLayout $ do
        setTitle "Sauípe Express|Contato"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead 
                [hamlet|
                    <meta charset="utf-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1">
                    <link rel="icon" href=@{StaticR imagens_icones_iconeCoqueiroFundo_png} type="image/x-icon">
                |]
        toWidget 
                [whamlet|
                    <nav class="navbar navbar-default navbar-static-top menu cor1"> 
                        <div class="container">
                            <div class="navbar-header">
                                <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span class="sr-only">Navegação
                                    <span class="icon-bar">
                                    <span class="icon-bar">
                                    <span class="icon-bar">
                                <a class="navbar-brand" href=@{HomeR}>Sauípe Express
                            <div class="collapse navbar-collapse" id="navbar-ex-collapse">
                                <ul class="nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href="../view/contato.html">Contato
                    <div class="section">
                        <header class="container">
                            <div class="row">
                                <div class="col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} class="img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div class="links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href="../view/contato.html"><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div class="section">
                        <div class="container fundo1">
                            <div class="row">
                                <div class="col-md-12">
                                    <h1>Contato
                                    <p>Entre em contato conosco por meio de um dos nossos canais de comunicação e feche negócio com a gente.
                                    <p>Nossa central de atendimento funciona de segunda a sexta-feira das 8h às 18h e aos sábados das 8h às 13h.
                                <div class="col-md-12">
                                  <div class="col-md-5">
                                    <h2>Localização
                                    <iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d3646.061387900585!2d-46.32303458501465!3d-23.958269284487358!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x94ce03a6e8657c03%3A0x8cc26677fac63151!2sAv.+Afonso+Pena%2C+45+-+Macuco%2C+Santos+-+SP%2C+11020-001!5e0!3m2!1spt-BR!2sbr!4v1450190185943" width="400" height="300" frameborder="0" style="border-radius:10px" allowfullscreen="" class="lado1 borda">
                                  <div class="col-md-7">
                                    <h2>Email
                                    <form role="form" method="post" action="http://www.sauipeexpress.com.br/email/">
                                      <div class="form-group">
                                        <div class="col-sm-12">
                                          <label for="nome" class="control-label">Nome
                                          <input type="text" class="form-control" id="nome" name="nome" placeholder="Digite seu nome">
                                      <div class="form-group">
                                        <div class="col-sm-12">
                                          <label for="email" class="control-label">Email
                                          <input type="email" class="form-control" id="email" name="email" placeholder="Digite seu email">
                                      <div class="form-group">
                                        <div class="col-sm-12">
                                          <label for="assunto" class="control-label">Assunto
                                          <input type="text" class="form-control" id="assunto" name="assunto" placeholder="Digite o assunto">
                                      <div class="form-group"> 
                                        <div class="col-sm-12">
                                          <label for="mensagem" class="control-label">Mensagem
                                          <textarea class="form-control" id="mensagem" name="mensagem" rows="7"  placeholder="Digite sua mensagem">
                                      <div class="form-group">
                                        <div class="col-sm-10 col-sm-offset-2 text-right">
                                          <input type="submit" value="Enviar">
                                          <input type="reset" value="Limpar">
                                <div class="col-md-12">
                                <div class="col-md-5">
                                    <h2>Endereço:
                                    Avenida Afonso Pena, 45 - Macuco, Santos - SP
                                <div class="col-md-7">
                                <h2>Telefones para contato:<br>
                                <p>Celular: (13)99747-7862<br>
                                Telefone: (13) 3223-9211 ou (13) 32245876<br>
                                NEXTEL(ID): 129*20237
                    <div class="section">
                        <footer class="container">
                            <div class="vcard row">
                                <div class="col-md-6">
                                    <h2 class="fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href="../view/contato.html">Contato
                                <div class="adr">Endereço:
                                    <br>
                                    <span class="street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span class="locality">Santos-
                                    <span class="region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" class="tel">Celular: (13)99747-7862
                                    <br>
                                    <span class="tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href="../view/contato.html" class="email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br
            |]

                
                

getServicosR :: Handler Html
getServicosR = defaultLayout $ do
        setTitle "Sauípe Express|Serviços"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead 
                [hamlet|
                    <meta charset="utf-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1">
                    <link rel="icon" href=@{StaticR imagens_icones_iconeCoqueiroFundo_png} type="image/x-icon">
                |]
        toWidget 
                [whamlet|
                    <nav class="navbar navbar-default navbar-static-top menu cor1"> 
                        <div class="container">
                            <div class="navbar-header">
                                <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span class="sr-only">Navegação
                                    <span class="icon-bar">
                                    <span class="icon-bar">
                                    <span class="icon-bar">
                                <a class="navbar-brand" href=@{HomeR}>Sauípe Express
                            <div class="collapse navbar-collapse" id="navbar-ex-collapse">
                                <ul class="nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href="../view/contato.html">Contato
                    <div class="section">
                        <header class="container">
                            <div class="row">
                                <div class="col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} class="img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div class="links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href="../view/contato.html"><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div class="section">
                        <div class="container fundo1">
                            <div class="row">
                                <div class="col-md-12">
                                    <img class="hidden-xs lado1" src=@{StaticR imagens_motoLogo1_png}>
                                    <h1>Serviços
                                    <p>Ao contratar os nossos serviços você agilizará suas entregas e retiradas
                                        de malotes, duplicatas, notas fiscais, serviços, bancários, pequenos e
                                        médios volumes entre outros, com total rapidez e segurança, pois contamos
                                        com uma equipe especializada para cada tipo de serviço, personalizando
                                        o atendimento para a necessidade de cada cliente, em diversos segmentos
                                        como:
                                    <p>Indústrias, bancos, financeiras, consultórios, concessionárias, contabilidades,
                                        advocacias, farmácias, restaurantes, deliverys , e todos estabelescimentos
                                        ou empresas que buscam uma solução definitiva e econômica em transportes
                                        urgentes.
                                    <h2>Mensal
                                    <p>Colocamos à disposição de sua empresa, um ou mais Motofretistas, uniformizados
                                        e equipados com CELULAR ou RADIOCOMUNICAÇÃO que servirão a sua empresa
                                        em qualquer tipo de serviço externo nos horários determinados em contrato.
                                    <h3>Integral
                                    <p>Segunda à sexta feira jornada de 8h/dia.
                                    <img class="hidden-xs lado2" src=@{StaticR imagens_montanaLogo1_png}>
                                    <h3>Meio Período(Manhã)
                                    <p>Jornada de 4h/dia Obs.: Em caso de algum imprevisto a substituição do
                                        condutor ou veículo será efetuada no maximo em 90 minutos.
                                    <h2>Esporádico
                                    <p>Serviços de entregas rápidas de encomendas leves de um ponto para o outro
                                        da cidade, este serviço atende a pessoas físicas e jurídicas, com rapidez
                                        e qualidade. O valor do serviço é definido previamente de acordo com a
                                        tabela de localidades, em função da distância.
                                    <p>
                                        <a href="../view/contato.html">Entre em contato conosco e veja os detalhes deste tipo de serviço
                    <div class="section">
                        <footer class="container">
                            <div class="vcard row">
                                <div class="col-md-6">
                                    <h2 class="fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href="../view/contato.html">Contato
                                <div class="adr">Endereço:
                                    <br>
                                    <span class="street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span class="locality">Santos-
                                    <span class="region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" class="tel">Celular: (13)99747-7862
                                    <br>
                                    <span class="tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href="../view/contato.html" class="email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br
            |]

getQuemSomosR :: Handler Html
getQuemSomosR = defaultLayout $ do
        setTitle "Sauípe Express|QuemSomos"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead 
                [hamlet|
                    <meta charset="utf-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1">
                    <link rel="icon" href=@{StaticR imagens_icones_iconeCoqueiroFundo_png} type="image/x-icon">
                |]
        toWidget 
            [whamlet|
                    <nav class="navbar navbar-default navbar-static-top menu cor1"> 
                        <div class="container">
                            <div class="navbar-header">
                                <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span class="sr-only">Navegação
                                    <span class="icon-bar">
                                    <span class="icon-bar">
                                    <span class="icon-bar">
                                <a class="navbar-brand" href=@{HomeR}>Sauípe Express
                            <div class="collapse navbar-collapse" id="navbar-ex-collapse">
                                <ul class="nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href="../view/contato.html">Contato
                    <div class="section">
                        <header class="container">
                            <div class="row">
                                <div class="col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} class="img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div class="links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href="../view/contato.html"><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div class="section">
                        <div class="container fundo1">
                            <div class="row">
                                <div class="col-md-12">
                                    <img class="hidden-xs lado2" src=@{StaticR imagens_caminhaoLogo_png}>
                                    <h1>Quem Somos
                                    <p>A SAUÍPE EXPRESS é uma empresa especializada em serviços de transportes,
                                        coletas e distribuição, que atua na Baixada Santista e Grande São Paulo
                                        e também em todo território nacional, com uma experiência de mais de 10
                                        anos atendendo pequenas, médias e grandes empresas, dos mais diversos segmentos
                                        da economia.
                                    <h2>Missão da Empresa
                                    <p>Nosso objetivo é atender empresas e particulares de todo porte, que precisam
                                        de agilidade e confiança no transporte de documentos e encomendas com motoboy
                                        24 horas, 7 dias semanais para que suas entregas e distribuições cheguem
                                        ao destino final de forma segura, rápida e eficiente, facilitando assim
                                        a vida do nosso cliente.
                                    <p>RAPIDEZ, RESPONSABILIDADE e CONFIANÇA !!!
                                    <p>Este é o nosso negócio. Seja nosso parceiro você também!!
                                    <h2>Localização
                                    <h3>Endereço
                                    <p>Av. Afonso Pena, 45 - Macuco, Santos - SP
                                    <iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d3646.061387900585!2d-46.32303458501465!3d-23.958269284487358!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x94ce03a6e8657c03%3A0x8cc26677fac63151!2sAv.+Afonso+Pena%2C+45+-+Macuco%2C+Santos+-+SP%2C+11020-001!5e0!3m2!1spt-BR!2sbr!4v1450190185943" width="400" height="300" frameborder="0" style="border-radius:10px" allowfullscreen="">
                    <div class="section">
                        <footer class="container">
                            <div class="vcard row">
                                <div class="col-md-6">
                                    <h2 class="fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href="../view/contato.html">Contato
                                <div class="adr">Endereço:
                                    <br>
                                    <span class="street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span class="locality">Santos-
                                    <span class="region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" class="tel">Celular: (13)99747-7862
                                    <br>
                                    <span class="tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href="../view/contato.html" class="email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br
            |]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
            setTitle "Sauípe Express|Home"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead [hamlet|
                    <meta charset="utf-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1">
                    <link rel="icon" href=@{StaticR imagens_icones_iconeCoqueiroFundo_png} type="image/x-icon">
            |]
            [whamlet|
                    <nav class="navbar navbar-default navbar-static-top menu cor1"> 
                        <div class="container">
                            <div class="navbar-header">
                                <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span class="sr-only">Navegação
                                    <span class="icon-bar">
                                    <span class="icon-bar">
                                    <span class="icon-bar">
                                <a class="navbar-brand" href=@{HomeR}>Sauípe Express
                            <div class="collapse navbar-collapse" id="navbar-ex-collapse">
                                <ul class="nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href="../view/contato.html">Contato
                    <div class="section">
                        <header class="container">
                            <div class="row">
                                <div class="col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} class="img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div class="links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href="../view/contato.html"><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div class="section">
                        <div class="container fundo1">
                            <div class="row">
                                <div class="col-md-12">
                                    <h1 class="central">Seja Bem Vindo ao nosso site!
                                    <p>No ano que fazemos nosso aniversário de 10 anos, reformulamos nosso site para melhor atendê-lo.
                                    <img src=@{StaticR imagens_designResponsivoLogo_png} alt="Dispositivos móveis(Design Responsivo)" title="Dispositivos móveis(Design Responsivo)" class="center-block hidden-xs">
                                    <p>Agora com o site totalmente responsivo, facilitando seu acesso através do Computador, Notebook, Tablet, celular e demais dispositivos móveis
                    <div class="section">
                        <footer class="container">
                            <div class="vcard row">
                                <div class="col-md-6">
                                    <h2 class="fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href="../view/contato.html">Contato
                                <div class="adr">Endereço:
                                    <br>
                                    <span class="street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span class="locality">Santos-
                                    <span class="region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" class="tel">Celular: (13)99747-7862
                                    <br>
                                    <span class="tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href="../view/contato.html" class="email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br
            |] 

main :: IO ()
main =  do 
    t@(Static settings) <- static "static"
    warp 8080 (SauipeExpress t)


