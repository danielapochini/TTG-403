{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Yesod.Static
import Yesod.Form.Bootstrap3
import Control.Monad.Logger (runStdoutLoggingT)


{-- tipo SauipeExpress com um data Constructor SauipeExpress + record syntax --} 
data SauipeExpress = SauipeExpress {getStatic :: Static, connPool :: ConnectionPool}

{-- o tipo SauipeExpress é uma instancia da classe Yesod, definida na biblioteca Yesod. 
 Yesod significa fundação em Hebreu, entao Pagina forma a fundação de nosso website. --}

instance Yesod SauipeExpress

-- tabela BD 
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuarios json
   nome Text 
   login Text
   senha Text
   deriving Show
|]

staticFiles "static" -- pasta static 

{--
quando o identificador do arquivo estatico é gerado 
(ex, static/logo.png ele se torna logo_png)
--}

{-- caminho da rota, nome da rota (Data Constructor), metodo de requisição --}
mkYesod "SauipeExpress" [parseRoutes|
/ HomeR GET 
/quemsomos QuemSomosR GET
/servicos ServicosR GET 
/contato ContatoR GET 
/cadastro UsuarioR GET POST
-- /cadastro/action/#UsuariosId ActionR GET PUT DELETE 
/cadastro/checar/#UsuariosId ChecarR GET
/erro ErroR GET
/static StaticR Static getStatic 
|]

{--
Para acessar o BD é preciso criar uma instancia YesodPersist, que diz
qual backend estamos usando e como executar uma ação
--}

instance YesodPersist SauipeExpress where
   type YesodPersistBackend SauipeExpress = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

-- conexão com o BD          
connStr = "dbname=dc4os3bfc24nle host=ec2-23-21-165-201.compute-1.amazonaws.com user=mbyfpmgifqzsml password=tx6KvtIZYlmna5zW4D4ISJ_FNU"

-- renderiza a pagina no idioma de quem a acessa
instance RenderMessage SauipeExpress FormMessage where
    renderMessage _ _ = defaultFormMessage
    
type Form a = Html -> MForm Handler (FormResult a, Widget)

-- Sempre que preciso um form, sera necessario funcoes deste tipo
usuarioForm :: Form Usuarios 
usuarioForm = renderDivs $ Usuarios <$>  -- coloca Usuarios pra dentro da Monad Form 
       --renderDivs: encapsular cada entidade do formulario dentro de uma div
       areq textField "Nome: " Nothing <*>
       -- <*> pq é uma função areq joga tudo pra dentro de Form
       areq textField "Login: " Nothing <*>
        -- Nothing pq o campo começa vazio
       areq passwordField "Senha: " Nothing 
       
getUsuarioR :: Handler Html
getUsuarioR = do 
        -- Gera o formulario para ser exibido
        (widget, enctype) <- generateFormPost usuarioForm
        defaultLayout $ do 
        setTitle "Sauípe Express|Cadastro"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidget [cassius|
                label 
                    font: 20px "typewriter", sans-serif;
                input
                   color:black;
                   font-size: 18px "typewriter", sans-serif;
                   margin-top: 20px;
                h1
                   text-align: center;
                   font-weight: bold;
                   font: 30px "typewriter", sans-serif;
           |]
        toWidgetHead [hamlet|
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1">
                <link rel="icon" href=@{StaticR imagens_icones_iconeCoqueiroFundo_png} type="image/x-icon">
        |]
        [whamlet| 
                    <nav ."navbar navbar-default navbar-static-top menu cor1"> 
                        <div ."container">
                            <div ."navbar-header">
                                <button type="button" ."navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span ."sr-only">Navegação
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                <a ."navbar-brand" href=@{HomeR}>Sauípe Express
                            <div ."collapse navbar-collapse" #"navbar-ex-collapse">
                                <ul ."nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href=@{ContatoR}>Contato
                    <div ."section">
                        <header ."container">
                            <div ."row">
                                <div ."col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} ."img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div ."links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href=@{ContatoR}><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div ."section">
                        <div ."container fundo1">
                            <div ."row">
                                <div ."col-md-12">
                                    <h1>Cadastro de Usuário
                                        <form method=post enctype=#{enctype} action=@{UsuarioR}>
                                            ^{widget}
                                            <input type="submit" value="Cadastrar" #"cadastrar">
                    <div ."section">
                        <footer ."container">
                            <div ."vcard row">
                                <div ."col-md-6">
                                    <h2 ."fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href=@{ContatoR}>Contato
                                <div ."adr">Endereço:
                                    <br>
                                    <span ."street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span ."locality">Santos-
                                    <span ."region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" ."tel">Celular: (13)99747-7862
                                    <br>
                                    <span ."tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href=@{ContatoR} ."email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br                        
        |]
        

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost usuarioForm
           case result of 
               FormSuccess prod -> (runDB $ insert prod) >>= \piid -> redirect (ChecarR piid)
               _ -> redirect ErroR


-- mensagem de erro caso não consiga cadastrar
getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    <p>não foi possivel cadastrar, tente novamente
|]

-- pega o ID e devolve o nome/usuario/senha cadastrado do BD para a pagina 
getChecarR :: UsuariosId -> Handler Html
getChecarR pid = do
    usu <- runDB $ get404 pid
    defaultLayout $ do 
        setTitle "Sauípe Express|Checar Cadastro"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidget [cassius|
                #user h1
                   text-align: center;
                   font-weight: bold;
                   font: 30px "typewriter", sans-serif;
                #user p
                   text-align: center;
                   font: 20px "typewriter", sans-serif;
        |]
        toWidgetHead [hamlet|
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1">
                <link rel="icon" href=@{StaticR imagens_icones_iconeCoqueiroFundo_png} type="image/x-icon">
        |]
        [whamlet| 
                    <nav ."navbar navbar-default navbar-static-top menu cor1"> 
                        <div ."container">
                            <div ."navbar-header">
                                <button type="button" ."navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span ."sr-only">Navegação
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                <a ."navbar-brand" href=@{HomeR}>Sauípe Express
                            <div ."collapse navbar-collapse" #"navbar-ex-collapse">
                                <ul ."nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href=@{ContatoR}>Contato
                    <div ."section">
                        <header ."container">
                            <div ."row">
                                <div ."col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} ."img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div ."links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href=@{ContatoR}><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div ."section">
                        <div ."container fundo1">
                            <div ."row">
                                <div ."col-md-12" #"user">
                                            <h1> Checar Usuário Cadastrado 
                                            <br>
                                            <p>Nome:  #{usuariosNome  usu}
                                            <p>Login: #{usuariosLogin usu}  
                                            <p>Senha: #{usuariosSenha usu}
                                            <br>
                    <div ."section">
                        <footer ."container">
                            <div ."vcard row">
                                <div ."col-md-6">
                                    <h2 ."fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href=@{ContatoR}>Contato
                                <div ."adr">Endereço:
                                    <br>
                                    <span ."street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span ."locality">Santos-
                                    <span ."region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" ."tel">Celular: (13)99747-7862
                                    <br>
                                    <span ."tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href=@{ContatoR} ."email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br                        
        |]


{-- 
--  lista todas os usuarios no banco a partir do nome
getUsuarioR :: Handler ()
getUsuarioR = do
    allUsuarios <- runDB $ selectList [] [Asc UsuariosNome]
    sendResponse (object [pack "data" .= fmap toJSON allUsuarios])

-- cria usuario no banco 
postUsuarioR :: Handler ()
postUsuarioR = do
    usuarios <- requireJsonBody :: Handler Usuarios
    runDB $ insert usuarios
    sendResponse (object [pack "resp" .= pack "CRIADO"]) 
    
    
-- se o usuario nao existir, dá erro 404
getActionR :: UsuariosId -> Handler ()
getActionR pid = do
    usu <- runDB $ get404 pid
    sendResponse $ toJSON usu
 
-- atualiza um usuario 
putActionR :: UsuariosId -> Handler ()
putActionR pid = do
    usu <- requireJsonBody :: Handler Usuarios
    runDB $ update pid [UsuariosNome =. usuariosNome usu]
    sendResponse (object [pack "resp" .= pack "ATUALIZADO"])

-- deleta um usuario pelo ID 
deleteActionR :: UsuariosId -> Handler ()
deleteActionR pid = do
    runDB $ delete pid
    sendResponse (object [pack "resp" .= pack "DELETADO"])     
--}    
  
----------------VIEW: Páginas do Site, Home, Quem Somos, Serviços, Contato-----------------------

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
                    <nav ."navbar navbar-default navbar-static-top menu cor1"> 
                        <div ."container">
                            <div ."navbar-header">
                                <button type="button" ."navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span ."sr-only">Navegação
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                <a ."navbar-brand" href=@{HomeR}>Sauípe Express
                            <div ."collapse navbar-collapse" #"navbar-ex-collapse">
                                <ul ."nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href=@{ContatoR}>Contato
                    <div ."section">
                        <header ."container">
                            <div ."row">
                                <div ."col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} ."img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div ."links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href=@{ContatoR}><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div ."section">
                        <div ."container fundo1">
                            <div ."row">
                                <div ."col-md-12">
                                    <h1 ."central">Seja Bem Vindo ao nosso site!
                                    <p>No ano que fazemos nosso aniversário de 10 anos, reformulamos nosso site para melhor atendê-lo.
                                    <img src=@{StaticR imagens_designResponsivoLogo_png} alt="Dispositivos móveis(Design Responsivo)" title="Dispositivos móveis(Design Responsivo)" ."center-block hidden-xs">
                                    <p>Agora com o site totalmente responsivo, facilitando seu acesso através do Computador, Notebook, Tablet, celular e demais dispositivos móveis
                    <div ."section">
                        <footer ."container">
                            <div ."vcard row">
                                <div ."col-md-6">
                                    <h2 ."fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href=@{ContatoR}>Contato
                                <div ."adr">Endereço:
                                    <br>
                                    <span ."street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span ."locality">Santos-
                                    <span ."region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" ."tel">Celular: (13)99747-7862
                                    <br>
                                    <span ."tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href=@{ContatoR} ."email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br
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
                    <nav ."navbar navbar-default navbar-static-top menu cor1"> 
                        <div ."container">
                            <div ."navbar-header">
                                <button type="button" ."navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span ."sr-only">Navegação
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                <a ."navbar-brand" href=@{HomeR}>Sauípe Express
                            <div ."collapse navbar-collapse" #"navbar-ex-collapse">
                                <ul ."nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href=@{ContatoR}>Contato
                    <div ."section">
                        <header ."container">
                            <div ."row">
                                <div ."col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} ."img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div ."links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href=@{ContatoR}><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div ."section">
                        <div ."container fundo1">
                            <div ."row">
                                <div ."col-md-12">
                                    <img ."hidden-xs lado2" src=@{StaticR imagens_caminhaoLogo_png}>
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
                    <div ."section">
                        <footer ."container">
                            <div ."vcard row">
                                <div ."col-md-6">
                                    <h2 ."fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href=@{ContatoR}>Contato
                                <div ."adr">Endereço:
                                    <br>
                                    <span ."street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span ."locality">Santos-
                                    <span ."region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" ."tel">Celular: (13)99747-7862
                                    <br>
                                    <span ."tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href=@{ContatoR} ."email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br
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
                    <nav ."navbar navbar-default navbar-static-top menu cor1"> 
                        <div ."container">
                            <div ."navbar-header">
                                <button type="button" ."navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span ."sr-only">Navegação
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                <a ."navbar-brand" href=@{HomeR}>Sauípe Express
                            <div ."collapse navbar-collapse" #"navbar-ex-collapse">
                                <ul ."nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href=@{ContatoR}>Contato
                    <div ."section">
                        <header ."container">
                            <div ."row">
                                <div ."col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} ."img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div ."links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href=@{ContatoR}><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div ."section">
                        <div ."container fundo1">
                            <div ."row">
                                <div ."col-md-12">
                                    <img ."hidden-xs lado1" src=@{StaticR imagens_motoLogo1_png}>
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
                                    <img ."hidden-xs lado2" src=@{StaticR imagens_montanaLogo1_png}>
                                    <h3>Meio Período(Manhã)
                                    <p>Jornada de 4h/dia Obs.: Em caso de algum imprevisto a substituição do
                                        condutor ou veículo será efetuada no maximo em 90 minutos.
                                    <h2>Esporádico
                                    <p>Serviços de entregas rápidas de encomendas leves de um ponto para o outro
                                        da cidade, este serviço atende a pessoas físicas e jurídicas, com rapidez
                                        e qualidade. O valor do serviço é definido previamente de acordo com a
                                        tabela de localidades, em função da distância.
                                    <p>
                                        <a href=@{ContatoR}>Entre em contato conosco e veja os detalhes deste tipo de serviço
                    <div ."section">
                        <footer ."container">
                            <div ."vcard row">
                                <div ."col-md-6">
                                    <h2 ."fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href=@{ContatoR}>Contato
                                <div ."adr">Endereço:
                                    <br>
                                    <span ."street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span ."locality">Santos-
                                    <span ."region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" ."tel">Celular: (13)99747-7862
                                    <br>
                                    <span ."tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href=@{ContatoR} ."email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br
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
                    <nav ."navbar navbar-default navbar-static-top menu cor1"> 
                        <div ."container">
                            <div ."navbar-header">
                                <button type="button" ."navbar-toggle" data-toggle="collapse" data-target="#navbar-ex-collapse">
                                    <span ."sr-only">Navegação
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                <a ."navbar-brand" href=@{HomeR}>Sauípe Express
                            <div ."collapse navbar-collapse" #"navbar-ex-collapse">
                                <ul ."nav navbar-nav navbar-right">
                                    <li>
                                        <a href=@{HomeR}>Home
                                    <li>
                                        <a href=@{QuemSomosR}>Quem Somos
                                    <li>
                                        <a href=@{ServicosR}>Serviços
                                    <li>
                                        <a href=@{ContatoR}>Contato
                    <div ."section">
                        <header ."container">
                            <div ."row">
                                <div ."col-md-6">
                                    <img src=@{StaticR imagens_logotipo10anos_png} ."img-responsive logotipo">
                                    <p>Frete e entregas para todo o Brasil.
                                <div ."links">
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_tel_png} alt="Telefone para contato" title="Telefone para contato Sauípe Express"> Telefone:(13)3223-9211 ou 3224-5876
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_whatsapp_png} alt="Celular para contato" title="Celular para contato Sauípe Express"> Celular:(13)99747-7862
                                    <p>
                                        <img src=@{StaticR imagens_icones_icone_cel_png} alt="ID nextel" title="ID Nextel Sauípe Express"> Nextel(ID):129*20237
                                    <p>
                                        <a href="https://www.facebook.com/armando.barros.5661?fref=pb&amp;hc_location=profile_browser" title="página do facebook Sauípe Express"><img src=@{StaticR imagens_icones_icone_facebook_png} alt="página do facebook"> Curta nossa página no facebook
                                    <p>
                                        <a href=@{ContatoR}><img src=@{StaticR imagens_icones_icone_email_png} alt="email para contato" title="Email para contato Sauípe Express"> contato@sauipeexpress.com.br
                    <div ."section">
                        <div ."container fundo1">
                            <div ."row">
                                <div ."col-md-12">
                                    <h1>Contato
                                    <p>Entre em contato conosco por meio de um dos nossos canais de comunicação e feche negócio com a gente.
                                    <p>Nossa central de atendimento funciona de segunda a sexta-feira das 8h às 18h e aos sábados das 8h às 13h.
                                <div ."col-md-12">
                                  <div ."col-md-5">
                                    <h2>Localização
                                    <iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d3646.061387900585!2d-46.32303458501465!3d-23.958269284487358!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x94ce03a6e8657c03%3A0x8cc26677fac63151!2sAv.+Afonso+Pena%2C+45+-+Macuco%2C+Santos+-+SP%2C+11020-001!5e0!3m2!1spt-BR!2sbr!4v1450190185943" width="400" height="300" frameborder="0" style="border-radius:10px" allowfullscreen="" ."lado1 borda">
                                  <div ."col-md-7">
                                    <h2>Email
                                    <form role="form" method="post" action="http://www.sauipeexpress.com.br/email/">
                                      <div ."form-group">
                                        <div ."col-sm-12">
                                          <label for="nome" ."control-label">Nome
                                          <input type="text" ."form-control" #"nome" name="nome" placeholder="Digite seu nome">
                                      <div ."form-group">
                                        <div ."col-sm-12">
                                          <label for="email" ."control-label">Email
                                          <input type="email" ."form-control" #"email" name="email" placeholder="Digite seu email">
                                      <div ."form-group">
                                        <div ."col-sm-12">
                                          <label for="assunto" ."control-label">Assunto
                                          <input type="text" ."form-control" #"assunto" name="assunto" placeholder="Digite o assunto">
                                      <div ."form-group"> 
                                        <div ."col-sm-12">
                                          <label for="mensagem" ."control-label">Mensagem
                                          <textarea ."form-control" #"mensagem" name="mensagem" rows="7"  placeholder="Digite sua mensagem">
                                      <div ."form-group">
                                        <div ."col-sm-10 col-sm-offset-2 text-right">
                                          <input type="submit" value="Enviar">
                                          <input type="reset" value="Limpar">
                                <div ."col-md-12">
                                <div ."col-md-5">
                                    <h2>Endereço:
                                    Avenida Afonso Pena, 45 - Macuco, Santos - SP
                                <div ."col-md-7">
                                <h2>Telefones para contato:<br>
                                <p>Celular: (13)99747-7862<br>
                                Telefone: (13) 3223-9211 ou (13) 32245876<br>
                                NEXTEL(ID): 129*20237
                    <div ."section">
                        <footer ."container">
                            <div ."vcard row">
                                <div ."col-md-6">
                                    <h2 ."fn">Sauípe Express Transportes Rápidos Ltda.
                                    <h3>Mapa do Site
                                    <a href=@{HomeR}>Home|
                                    <a href=@{QuemSomosR}>Quem Somos|
                                    <a href=@{ServicosR}>Serviços|
                                    <a href=@{ContatoR}>Contato
                                <div ."adr">Endereço:
                                    <br>
                                    <span ."street-addresss">Av. Afonso Pena, 45 - Macuco,
                                    <br>
                                    <span ."locality">Santos-
                                    <span ."region">SP
                                    <br>
                                    <span title="Celular Sauípe Express" ."tel">Celular: (13)99747-7862
                                    <br>
                                    <span ."tel" title="Telefone Sauípe Express">Telefone: (13)3223-9211 ou 3224-5876 <br>Nextel(ID):129*20237
                                    <br>
                                <a href=@{ContatoR} ."email" alt="Email Sauípe Express" title="Link para página de contato"> contato@sauipeexpress.com.br
            |]

main :: IO ()
main = do
       s@(Static settings) <- static "static"    
       runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (SauipeExpress s pool)