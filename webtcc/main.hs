{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Text (Text, pack) 
import Text.Julius (Javascript) 
import Text.Lucius (Css) 
import Yesod.Static
import Yesod.Form.Bootstrap3
import Control.Monad.Logger (runStdoutLoggingT)


{-- tipo SauipeExpress com um data Constructor SauipeExpress + record syntax --} 
data SauipeExpress = SauipeExpress {getStatic :: Static, connPool :: ConnectionPool}

{-- o tipo SauipeExpress é uma instancia da classe Yesod, definida na biblioteca Yesod. 
 Yesod significa fundação em Hebreu, entao Pagina forma a fundação de nosso website. --}

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
/admin AdminR GET
/cadastro UsuarioR GET POST
/contato ContatoR GET
/erro ErroR GET
/geolocalizacao GeolocalizacaoR GET
/login LoginR GET POST
/logout LogoutR GET
/perfil/#UsuariosId PerfilR GET
/quemsomos QuemSomosR GET
/servicos ServicosR GET
/static StaticR Static getStatic
|]



--A função 'isAuthorized' determina os acessos por rota 
instance Yesod SauipeExpress where 
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _     = return Authorized
    isAuthorized ErroR _      = return Authorized
    isAuthorized UsuarioR _   = return Authorized
    isAuthorized HomeR _      = return Authorized
    isAuthorized QuemSomosR _ = return Authorized
    isAuthorized ServicosR _  = return Authorized
    isAuthorized ContatoR _   = return Authorized 
    isAuthorized AdminR _     = isAdmin
    isAuthorized _ _          = isUser

--Autenticação do Admin
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing      -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _       -> Unauthorized "Você precisa ser Admin para ter acesso a essa área!"

--A função isUser faz a autenticação do Usuário
isUser = do
    -- 'lookupSession' verifica se há session, e atribui à 'mu'
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _  -> Authorized

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

--Abaixo, criamos o Form com uma Tupla de dois Text, pois queremos acessar apenas os campos Login e Senha de usuarios,
--Mas NÃO queremos o campo Nome (Senão bastaria usar o formUser acima) 
loginForm :: Form (Text,Text)
loginForm = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing
           

getLoginR :: Handler Html
getLoginR = do
        (widget, enctype) <- generateFormPost loginForm
        defaultLayout $ do 
        setTitle "Sauípe Express|Login"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidget [cassius|
                label 
                    font: 20px "typewriter", sans-serif;
                    display: inline-block;
                    width: 23%; 
                input
                   color:black; 
                   font: 18px "typewriter", sans-serif;
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
        toWidget[whamlet|
                <div ."section">
                    <header ."container">
                        <div ."row">
                            <div ."col-md-6">
                                <h2 ."central"> LOGIN
                                <img src=@{StaticR imagens_logotipo10anos_png} ."img-responsive logotipo">
                            <div ."col-md-6">
                                <h2>Preencha os campos abaixo:
                                <form method=post enctype=#{enctype} action=@{LoginR}>
                                 ^{widget}
                                 <input type="submit" value="Login">
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
                    display: inline-block;
                    width: 23%; 
                input
                   color:black; 
                   font: 18px "typewriter", sans-serif;
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
        toWidget[whamlet|
                <div ."section">
                    <header ."container">
                        <div ."row">
                            <div ."col-md-6">
                                <h2 ."central"> CADASTRO DE USUÁRIO
                                <img src=@{StaticR imagens_logotipo10anos_png} ."img-responsive logotipo">
                            <div ."col-md-6">
                                <h2>Preencha o cadastro
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
        
--'Post' dos campos do login para a autenticação do usuário
--Obs.: Funções do Banco de Dados SEMPRE têm o runDB
postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost loginForm
           case result of 
               --Caso seja Admin:
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               --Caso seja Usuário Comum:
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsuariosLogin ==. login, UsuariosSenha ==. senha] []
                   case user of
                       --Caso o User venha 'vazio'            
                       Nothing -> redirect LoginR
                       --Caso o user seja retornado com sucesso, setamos a sessão e redirecionamos para a HomeR
                       --Abaixo: "pid" é o ID, e "u" contém todos os outros campos do registro
                       --A session é setada com o id do usuário
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)
               _ -> redirect ErroR --Em caso de erro, redirect para ErroR
               
postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost usuarioForm
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR


getPerfilR :: UsuariosId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout [whamlet|
          <p><b> Pagina de #{usuariosNome user}
          <p><b> Login: #{usuariosLogin user}
      |]

-- Pagina de Erro 
getErroR :: Handler Html
getErroR = defaultLayout $ do  
            setTitle "Sauípe Express"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead [hamlet|
                    <meta charset="utf-8" HTTP-EQUIV="refresh" CONTENT="5;URL=@{HomeR}">
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
                                  <h1>Desculpe! 
                                  <img .="img-responsive" src=@{StaticR imagens_erro1_png}>
                                  <h3>A página que você digitou não existe ou está fora do ar. 
                                  <p>Em instantes você será redirecionada para nossa página principal.
                                  <br> Obrigado!
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
 

-- Pagina apenas para Admin 
getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
     <h1> Bem-vindo!
|]
            
            
-- Pagina de Logout 
getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> ADEUS!
     |]

getQuemR :: Handler Html
getQuemR = do
     mu <- lookupSession "_ID"
     case mu of
        --Se em 'mu' houver sessão:
        Just sess -> do
            --Na Session é guardado um Text, mas só é possível converter de Text para String (unpack), e de String para Int (read)
            --O 'toSqlKey' converte de Int para Key(do BD)

            --(toSqlKey $ read $ unpack sess) <--- Transforma a Session de Text pra String, de String pra Inteiro e de Int pra chave
            uid <- return (toSqlKey $ read $ unpack sess) :: Handler (Key Usuarios)
            user <- runDB $ get404 uid
            defaultLayout [whamlet|
               <h1> Quem sou? #{usuariosNome user}
            |]
        --Se não houver (é Nothing):
        Nothing -> redirect ErroR
        
  
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
                    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0">
                    <link rel="icon" href=@{StaticR imagens_icones_iconeCoqueiroFundo_png} type="image/x-icon">
                |]
        toWidget [cassius|
    
                .googlemaps  
                    position: relative;
                    padding-bottom: 80%;  
                    height: 0;
                    overflow: hidden;
                
                .googlemaps iframe  
                    position: absolute;
                    top: 0;
                    left: 0;
                    width: 100% !important;
                    height: 100% !important;
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
                                <div ."col-md-12">
                                    <h1>Contato
                                    <p>Entre em contato conosco por meio de um dos nossos canais de comunicação e feche negócio com a gente.
                                    <p>Nossa central de atendimento funciona de segunda a sexta-feira das 8h às 18h e aos sábados das 8h às 13h.
                                <div ."col-md-12">
                                    <div ."col-md-5">
                                        <h2>Localização
                                        <div ."googlemaps">
                                        <iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d3646.061387900585!2d-46.32303458501465!3d-23.958269284487358!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x94ce03a6e8657c03%3A0x8cc26677fac63151!2sAv.+Afonso+Pena%2C+45+-+Macuco%2C+Santos+-+SP%2C+11020-001!5e0!3m2!1spt-BR!2sbr!4v1450190185943" width="400" height="300" frameborder="0" style="border-radius: 10px;" allowfullscreen="">
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
            
-------------- GEOLOCALIZACAO ---------------------

getGeolocalizacaoR :: Handler Html
getGeolocalizacaoR = defaultLayout $ do
        setTitle "Sauípe Express|Geolocalização"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        addScriptRemote "https://maps.googleapis.com/maps/api/js?key=AIzaSyDhcgn5SQ2cFSDA7jOTD1dwW4cX2oRiw4k"
        toWidget[julius|
            var map,
                currentPositionMarker,
                mapCenter = new google.maps.LatLng(-23.9581857, -46.3207414),
                map;
    
            function initializeMap()
            {
                map = new google.maps.Map(document.getElementById('map_canvas'), {
                   zoom: 16,
                   center: mapCenter,
                   mapTypeId: google.maps.MapTypeId.ROADMAP
                 });
                 
            }
     
            function locError(error) {
                alert("A posição não pode ser encontrada");
            }
           
            function setCurrentPosition(pos) {
                currentPositionMarker = new google.maps.Marker({
                    map: map,
                    position: new google.maps.LatLng(
                        pos.coords.latitude,
                        pos.coords.longitude
                    ),
                    title: "Posição Atual"
                });
                
                map.panTo(new google.maps.LatLng(
                        pos.coords.latitude,
                        pos.coords.longitude
                    ));
                    
                var infowindow = new google.maps.InfoWindow({
                content: 'Motoboy: Mauro'
            });
            infowindow.open(map,currentPositionMarker);
            }
     
            function displayAndWatch(position) {
             
                setCurrentPosition(position);
                watchCurrentPosition();
            }
     
            function watchCurrentPosition() {
                var positionTimer = navigator.geolocation.watchPosition(
                    function (position) {
                        setMarkerPosition(
                            currentPositionMarker,
                            position
                        );
                    });
            }
     
            function setMarkerPosition(marker, position) {
                marker.setPosition(
                    new google.maps.LatLng(
                        position.coords.latitude,
                        position.coords.longitude)
                );
            }
     
            function initLocationProcedure() {
                initializeMap();
                if (navigator.geolocation) {
                    navigator.geolocation.getCurrentPosition(displayAndWatch, locError);
                } else {
                    alert("Seu navegador não suporta o Geolocation API!");
                }
            }
      
            $(document).ready(function() {
                initLocationProcedure();
            });
        |]
        toWidget 
                [cassius|
                    .gm-style .gm-style-iw
                        color: #001f3f;
                |]
        toWidgetHead 
                [hamlet|
                    <meta charset="utf-8">
                    <meta name="viewport" content="width=device-width, initial-scale=1">
                    <link rel="icon" href=@{StaticR imagens_icones_iconeCoqueiroFundo_png} type="image/x-icon">
                |] 
        toWidget 
                [whamlet|
                    <h1>Localização Atual: 
                    <div #"map_canvas" style="height:25em; margin:0; padding:0;">
                |]

main :: IO ()
main = do
       s@(Static settings) <- static "static"    
       runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (SauipeExpress s pool)
       
       
       
       
{-
cd web2
    stack build

lsof -i:8080
kill -9 pid ????

curl https://webdev-manfi89.c9users.io/cadastro \
  -v \
  -X POST \
  -H 'Content-Type: application/json' \
  -d '{"nome":"Beatriz"}'
  
  
  curl https://webdev-manfi89.c9users.io/cadastro/delete/1 \
  -v \
  -X DELETE \
  
  curl https://webdev-manfi89.c9users.io/cadastro/update/2 \
  -v \
  -X PUT \
  -H 'Content-Type: application/json' \
  -d '{"nome":"Mauro"}'
-}