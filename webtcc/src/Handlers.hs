{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Routes
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Julius
import Text.Lucius 
import Text.Hamlet
import Text.Cassius 
import Yesod.Static

import Database.Persist.Postgresql


mkYesodDispatch "SauipeExpress" pRoutes


-- Sempre que preciso um form, sera necessario funcoes deste tipo
usuarioForm :: Form Usuarios 
usuarioForm = renderDivs $ Usuarios <$>  -- coloca Usuarios pra dentro da Monad Form 
       --renderDivs: encapsular cada entidade do formulario dentro de uma div
       areq textField "Nome: " Nothing <*>
       -- <*> pq é uma função areq joga tudo pra dentro de Form
       areq textField "Login: " Nothing <*>
        -- Nothing pq o campo começa vazio
       areq passwordField "Senha: " Nothing 

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost usuarioForm
           case result of 
               FormSuccess user -> (runDB $ insert user) >> redirect SucessoR
               _ -> redirect ErroR

--Abaixo, criamos o Form com uma Tupla de dois Text, pois queremos acessar apenas os campos Login e Senha de usuarios,
--Mas NÃO queremos o campo Nome (Senão bastaria usar o formUser acima) 
loginForm :: Form (Text,Text)
loginForm = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing
           
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
           

-- Página Login 
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
        toWidget $(cassiusFile "templates/cassius/form.cassius")
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(whamletFile "templates/whamlet/login.hamlet")     

-- Página Cadastro de Usuario 
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
        toWidget $(cassiusFile "templates/cassius/form.cassius")
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(whamletFile "templates/whamlet/usuario.hamlet") 
  
getPerfilR :: UsuariosId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do 
            setTitle "Sauípe Express|Funcionário"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addStylesheetRemote "https://api.mapbox.com/mapbox.js/v2.4.0/mapbox.css"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            addScriptRemote "https://cdn.firebase.com/js/client/2.2.1/firebase.js"
            addScriptRemote "https://api.tiles.mapbox.com/mapbox.js/v2.1.6/mapbox.js"
            toWidget $(juliusFile "templates/julius/geolocalizacao.julius") 
            toWidget $(cassiusFile "templates/cassius/funcionario.cassius")
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
            toWidget $(whamletFile "templates/whamlet/funcionario.hamlet") 

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
            toWidgetHead $(hamletFile "templates/hamlet/headhome.hamlet")
            toWidget $(whamletFile "templates/whamlet/error.hamlet") 

-- Pagina de Sucesso 
getSucessoR :: Handler Html
getSucessoR = defaultLayout $ do  
            setTitle "Sauípe Express"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "templates/hamlet/headadmin.hamlet")
            toWidget $(whamletFile "templates/whamlet/sucesso.hamlet") 
                
            
-- Pagina apenas para Admin 
getAdminR :: Handler Html
getAdminR = defaultLayout $ do 
            setTitle "Sauípe Express|Cadastro"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addStylesheetRemote "https://api.mapbox.com/mapbox.js/v2.4.0/mapbox.css"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            addScriptRemote "https://cdn.firebase.com/js/client/2.2.1/firebase.js"
            addScriptRemote "https://api.tiles.mapbox.com/mapbox.js/v2.1.6/mapbox.js"
            toWidget $(juliusFile "templates/julius/geoadmin.julius") 
            toWidget $(cassiusFile "templates/cassius/admin.cassius")
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
            toWidget $(whamletFile "templates/whamlet/admin.hamlet") 


-- Pagina de Logout 
getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout $ do
            setTitle "Sauípe Express"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "templates/hamlet/headhome.hamlet")
            toWidget $(whamletFile "templates/whamlet/logout.hamlet") 

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
        
  
----------------Páginas do Site - Home, Quem Somos, Serviços, Contato-----------------------

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
            setTitle "Sauípe Express|Home"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
            toWidget $(whamletFile "templates/whamlet/home.hamlet") 

getQuemSomosR :: Handler Html
getQuemSomosR = defaultLayout $ do
        setTitle "Sauípe Express|QuemSomos"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(whamletFile "templates/whamlet/quemsomos.hamlet")

getServicosR :: Handler Html
getServicosR = defaultLayout $ do
        setTitle "Sauípe Express|Serviços"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(whamletFile "templates/whamlet/servicos.hamlet")

getContatoR :: Handler Html
getContatoR = defaultLayout $ do
        setTitle "Sauípe Express|Contato"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/headcontato.hamlet")
        toWidget $(cassiusFile "templates/cassius/contato.cassius")
        toWidget $(whamletFile "templates/whamlet/contato.hamlet")