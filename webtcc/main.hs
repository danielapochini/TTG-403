{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Yesod.Static
import Control.Monad.Logger (runStdoutLoggingT)


data SauipeExpress = SauipeExpress {getStatic :: Static}

instance Yesod SauipeExpress

staticFiles "static"

mkYesod "SauipeExpress" [parseRoutes|
/ HomeR GET 
/static StaticR Static getStatic
--/quemsomos QuemSomosR GET
--/servicos ServicosR GET 
--/contato ContatoR GET 
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
                                        <a href="../view/quemsomos.html">Quem Somos
                                    <li>
                                        <a href="../view/servicos.html">Serviços
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
                                    <a href="../view/quemsomos.html">Quem Somos|
                                    <a href="../view/servicos.html">Serviços|
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


