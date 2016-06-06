{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, EmptyDataDecls, ViewPatterns #-}
 
module Handlers where
import Routes
import Yesod
import Database.Persist.Postgresql
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Data.Time
import qualified Data.Text as T
import Text.Julius
import Text.Lucius  
import Text.Hamlet
import Text.Cassius 
import Yesod.Form.Jquery
import Yesod.Static

import Database.Persist.Postgresql


mkYesodDispatch "SauipeExpress" pRoutes
 

----------------------- WIDGETS PERSONALIZADAS / FORMULARIO GENÉRICO + INTERNACIONALIZAÇÃO  ------------------------------
cliWid :: Widget
cliWid = [whamlet| 
    _{MsgAdmin1} 
|]

funcWid :: Widget
funcWid = [whamlet| 
    _{MsgAdmin2} 
|]

entWid :: Widget
entWid = [whamlet| 
    _{MsgEntrega} 
|]

filWid :: Widget
filWid = [whamlet| 
    _{MsgFilial} 
|]

logWid :: Widget
logWid = [whamlet| 
    _{MsgLogin1}
|]

------------------ ESTRUTURA DO SITE: HEADER/NAVEGAÇÃO/FOOTER---------------------
header :: Widget
header = $(whamletFile "templates/widgets/header.hamlet") 
{-- as informações de header e footer são as mesmas em todas as páginas, para não ter
retrabalho em escreeve-las em cada página nova, é só importar o widget 
usando ^{header} ou ^{footer} dentro da pagina html --}
footer :: Widget
footer = $(whamletFile "templates/widgets/footer.hamlet")   

widgetForm :: Route SauipeExpress -> Enctype -> Widget -> Widget -> Widget
-- função que gera formulários em forma genérica 
widgetForm x enctype widget novaWidget = [whamlet|
            <h1>
                ^{novaWidget}
            <form method=post action=@{x} enctype=#{enctype}>
                ^{widget}
                <input ."btn btn-primary" type="submit" value="_{MsgCadastroBtn}" #"cadastrar">
            <h3>_{MsgCadastro}
|]

-- formulário Filial
filialForm :: Form Filial 
filialForm = renderDivs $ Filial <$>   
       areq textField (fieldSettingsLabel MsgTxtNome) Nothing <*>  
       areq textField (fieldSettingsLabel MsgTxtCnpj) {fsAttrs = [("maxlength","14")]} Nothing <*> 
       areq textField (fieldSettingsLabel MsgTxtEndereco) Nothing <*> 
       areq textField (fieldSettingsLabel MsgTxtCidade) Nothing 
       
-- formulário Cliente 
clienteForm :: Form Cliente
clienteForm = renderDivs $ Cliente <$>   
       areq textField (fieldSettingsLabel MsgTxtNome) Nothing <*>  
       areq textField (fieldSettingsLabel MsgTxtCpf) {fsAttrs=[("maxlength","11")]} Nothing <*> 
       areq (jqueryDayField def { jdsChangeYear = True  
                 , jdsYearRange = "1900:1998"  
                  }) (fieldSettingsLabel MsgTxtNascimento) Nothing <*>       
       areq emailField (fieldSettingsLabel MsgTxtEmail) Nothing <*>
       areq textField (fieldSettingsLabel MsgTxtEndereco) Nothing <*> 
       areq textField (fieldSettingsLabel MsgTxtCidade) Nothing <*>          
       areq textField (fieldSettingsLabel MsgTxtTelefone) {fsAttrs=[("maxlength","11")]} Nothing      

-- formulário Entrega
entregaForm :: Form Entrega
entregaForm = renderDivs $ Entrega <$>   
       areq (selectField fili) (fieldSettingsLabel MsgTxtFilial) Nothing <*> 
       areq (selectField func) (fieldSettingsLabel MsgForm2) Nothing <*>
       areq (selectField cli) (fieldSettingsLabel MsgTxtCliente) Nothing <*>
       lift (liftIO getCurrentTime) <*>
       lift (liftIO $ return False)
       
-- formulário Usuarios
usuarioForm :: Form Usuarios 
usuarioForm = renderDivs $ Usuarios <$>   
       areq textField (fieldSettingsLabel MsgTxtNome) Nothing <*>  
       areq textField (fieldSettingsLabel MsgTxtLogin) Nothing <*> 
       areq passwordField (fieldSettingsLabel MsgTxtSenha) Nothing <*>
       areq (selectField $ optionsPairs [(MsgForm3, "Administrador"),(MsgForm2, "Funcionário")]) (fieldSettingsLabel MsgForm4) Nothing

--Abaixo, criamos o Form com uma Tupla de dois Text, pois queremos acessar apenas os campos Login e Senha de usuarios,
--Mas NÃO queremos o campo Nome (Senão bastaria usar o formusuario acima) 
loginForm :: Form (Text,Text)
loginForm = renderDivs $ (,) <$>
           areq textField (fieldSettingsLabel MsgTxtLogin) Nothing <*>
           areq passwordField (fieldSettingsLabel MsgTxtSenha) Nothing
----------------------------------------------------
fili = do
       entidades <- runDB $ selectList [] [Asc FilialNome] 
       optionsPairs $ fmap (\ent -> (filialNome $ entityVal ent, entityKey ent)) entidades

cli = do
       entidades <- runDB $ selectList [] [Asc ClienteNome] 
       optionsPairs $ fmap (\ent -> (clienteNome $ entityVal ent, entityKey ent)) entidades

func = do
       entidades <- runDB $ selectList [UsuariosTipo ==. "Funcionário"] [Asc UsuariosNome]
       optionsPairs $ fmap (\ent -> (usuariosNome $ entityVal ent, entityKey ent)) entidades  
--------------------- METODOS POST -----------------------------
postCadClienteR :: Handler Html
postCadClienteR = do
           ((result, _), _) <- runFormPost clienteForm
           case result of 
               FormSuccess cli -> (runDB $ insert cli) >> redirect SucessoR
               _ -> redirect ErroR
               
postCadEntregaR :: Handler Html
postCadEntregaR = do
           ((result, _), _) <- runFormPost entregaForm
           case result of 
               FormSuccess ent -> (runDB $ insert ent) >> redirect SucessoR
               _ -> redirect ErroR
               
postCadFilialR :: Handler Html
postCadFilialR = do
           ((result, _), _) <- runFormPost filialForm
           case result of 
               FormSuccess fili -> (runDB $ insert fili) >> redirect SucessoR
               _ -> redirect ErroR
               
postCadUsuarioR :: Handler Html
postCadUsuarioR = do
           ((result, _), _) <- runFormPost usuarioForm
           case result of 
               FormSuccess user -> (runDB $ insert user) >> redirect SucessoR
               _ -> redirect ErroR
   
postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost loginForm
           case result of 
               --Caso seja Admin:
                FormSuccess ("admin","admin") -> setSession "_ID" "Administrador" >> redirect AdminR
               --Caso seja Usuário Comum:
                FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsuariosLogin ==. login, UsuariosSenha ==. senha] []
                   case user of 
                       {--UsuariosTipo == "Administrador" -> setSession "_ID" "Administrador" >> redirect (AdminR)
                       UsuariosTipo == "Funcionário" -> setSession "_ID" "Funcionário" >> redirect (FuncionarioR) --}
                       
                       --Caso o User venha 'vazio'            
                       Nothing -> redirect LoginR
                       --Caso o user seja retornado com sucesso, setamos a sessão e redirecionamos para FuncionarioR
                       --Abaixo: "pid" é o ID, e "u" contém todos os outros campos do registro
                       --A session é setada com o id do usuário
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (FuncionarioR)
                _ -> redirect ErroR --Em caso de erro, redirect para ErroR
    
           
postPerfilR :: UsuariosId -> Handler Html
postPerfilR pid = do
     runDB $ delete pid
     redirect ListUsuarioR
                          
           
postPerfilFilialR :: FilialId -> Handler Html
postPerfilFilialR pid = do
     runDB $ delete pid
     redirect ListFilialR       
     
           
postPerfilClienteR :: ClienteId -> Handler Html
postPerfilClienteR pid = do
     runDB $ delete pid
     redirect ListClienteR    

---------------- Área Administrador -----------------------
            
-- Pagina apenas para Admin 
getAdminR :: Handler Html
getAdminR = defaultLayout $ do 
            setTitle "Sauípe Express|Painel Admin"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            -- addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addStylesheetRemote "https://api.mapbox.com/mapbox.js/v2.4.0/mapbox.css"
            addStylesheetRemote "https://api.tiles.mapbox.com/mapbox.js/plugins/leaflet-label/v0.2.1/leaflet.label.css"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            addScriptRemote "https://cdn.firebase.com/js/client/2.2.1/firebase.js"
            addScriptRemote "https://api.tiles.mapbox.com/mapbox.js/v2.1.6/mapbox.js"
            addScriptRemote "https://api.tiles.mapbox.com/mapbox.js/plugins/leaflet-label/v0.2.1/leaflet.label.js"
            toWidget $(luciusFile "templates/lucius/principal.lucius") 
            toWidget $(juliusFile "templates/julius/geoadmin.julius") 
            toWidget $(cassiusFile "templates/cassius/admin.cassius")
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
            toWidget $(whamletFile "templates/whamlet/admin.hamlet")  

--Página de Cadastro de Filial
getCadFilialR :: Handler Html
getCadFilialR = do  
        (widget, enctype) <- generateFormPost filialForm
        defaultLayout $ do 
        setTitle "Sauípe Express| Cadastro Filial Empresa"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css 
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/form.cassius")
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(whamletFile "templates/whamlet/cadastro/cadfilial.hamlet") 
        
getListFilialR :: Handler Html
getListFilialR = do
        filial <- runDB $ selectList [] [Asc FilialNome]
        defaultLayout $ do 
        setTitle "Sauípe Express|Lista de Filial"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css 
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/list.cassius") 
        toWidget $(juliusFile "templates/julius/list.julius")
        toWidget $(whamletFile "templates/whamlet/listagem/listfilial.hamlet")
         
        
--Página de Cadastro de Cliente
getCadClienteR :: Handler Html
getCadClienteR = do  
        (widget, enctype) <- generateFormPost clienteForm
        defaultLayout $ do 
        setTitle "Sauípe Express| Cadastro Cliente"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css 
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/form.cassius")
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(whamletFile "templates/whamlet/cadastro/cadcliente.hamlet")   

getListClienteR :: Handler Html        
getListClienteR = do
        cliente <- runDB $ selectList [] [Asc ClienteNome]
        defaultLayout $ do 
        setTitle "Sauípe Express|Lista de Filial"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css 
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/list.cassius") 
        toWidget $(juliusFile "templates/julius/list.julius")
        toWidget $(whamletFile "templates/whamlet/listagem/listcliente.hamlet")        
        
--Página de Cadastro de Entrega 
getCadEntregaR :: Handler Html
getCadEntregaR = do  
        (widget, enctype) <- generateFormPost entregaForm
        defaultLayout $ do 
        setTitle "Sauípe Express| Cadastro Entrega"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        -- addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/form.cassius")
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(whamletFile "templates/whamlet/cadastro/cadentrega.hamlet")    

getListEntregaR :: Handler Html        
getListEntregaR = do 
        entregas <- runDB $ (rawSql "SELECT ??, ??, ?? FROM entrega INNER JOIN cliente ON entrega.cliente_id=cliente.id INNER JOIN usuarios ON entrega.funcionario_id=usuarios.id" [])::Handler [(Entity Entrega, Entity Cliente, Entity Usuarios)] 
        defaultLayout $ do 
        setTitle "Sauípe Express|Lista Entregas"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css 
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/list.cassius") 
        toWidget $(juliusFile "templates/julius/list.julius")
        toWidget $(whamletFile "templates/whamlet/listagem/listentrega.hamlet") 

-- Página Cadastro de Funcionário 
getCadUsuarioR :: Handler Html
getCadUsuarioR = do  
        (widget, enctype) <- generateFormPost usuarioForm
        defaultLayout $ do 
        setTitle "Sauípe Express| Cadastro Funcionário"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        -- addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/form.cassius")
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(whamletFile "templates/whamlet/cadastro/cadusuario.hamlet") 

-- Lista Funcionário Cadastrado 
getListUsuarioR :: Handler Html
getListUsuarioR = do
        listaP <- runDB $ selectList [] [Asc UsuariosNome]
        defaultLayout $ do 
        setTitle "Sauípe Express|Lista"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        -- addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/list.cassius") 
        toWidget $(juliusFile "templates/julius/list.julius")
        toWidget $(whamletFile "templates/whamlet/listagem/listusuario.hamlet")
 
---------------- Login, Logout -----------------------

-- Página Login 
getLoginR :: Handler Html
getLoginR = do
        (widget, enctype) <- generateFormPost loginForm
        defaultLayout $ do 
        setTitle "Sauípe Express|Login"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        -- addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/form.cassius")
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(whamletFile "templates/whamlet/login.hamlet")    
        
 
-- Pagina de Logout 
getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout $ do
            setTitle "Sauípe Express"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            --addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "templates/hamlet/headhome.hamlet")
            toWidget $(luciusFile "templates/lucius/principal.lucius") 
            toWidget $(whamletFile "templates/whamlet/logout.hamlet") 
       
        
----------- PERFIL USUARIO -------------      

getPerfilR :: UsuariosId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do 
            setTitle "Sauípe Express|Funcionário"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            -- addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js" 
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"    
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet") 
            toWidget $(luciusFile "templates/lucius/principal.lucius") 
            toWidget $(juliusFile "templates/julius/perfil.julius") 
            toWidget $(whamletFile "templates/whamlet/perfil/perfil.hamlet") 
    
----------- PERFIL Cliente -------------      

getPerfilClienteR :: ClienteId -> Handler Html
getPerfilClienteR uid = do
      cliente <- runDB $ get404 uid
      defaultLayout $ do 
            setTitle "Sauípe Express|Cliente"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            -- addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js" 
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"    
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet") 
            toWidget $(luciusFile "templates/lucius/principal.lucius") 
            toWidget $(juliusFile "templates/julius/perfil.julius") 
            toWidget $(whamletFile "templates/whamlet/perfil/cliente.hamlet")     

----------- PERFIL Filial -------------      

getPerfilFilialR :: FilialId -> Handler Html
getPerfilFilialR uid = do
      filial <- runDB $ get404 uid
      defaultLayout $ do 
            setTitle "Sauípe Express|Filial"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            -- addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js" 
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"    
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet") 
            toWidget $(luciusFile "templates/lucius/principal.lucius") 
            toWidget $(juliusFile "templates/julius/perfil.julius") 
            toWidget $(whamletFile "templates/whamlet/perfil/filial.hamlet")     
        
--------------------------- FUNCIONARIO  --------------------
getFuncionarioR :: Handler Html
getFuncionarioR = do
     mu <- lookupSession "_ID"
     case mu of
        --Se em 'mu' houver sessão:
        Just sess -> do
            --Na Session é guardado um Text, mas só é possível converter de Text para String (unpack), e de String para Int (read)
            --O 'toSqlKey' converte de Int para Key(do BD)

            --(toSqlKey $ read $ unpack sess) <--- Transforma a Session de Text pra String, de String pra Inteiro e de Int pra chave
            uid <- return (toSqlKey $ read $ unpack sess) :: Handler (Key Usuarios)
            user <- runDB $ get404 uid
            defaultLayout $ do 
            setTitle "Sauípe Express|Funcionário"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            --addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addStylesheetRemote "https://api.mapbox.com/mapbox.js/v2.4.0/mapbox.css"
            addStylesheetRemote "https://api.tiles.mapbox.com/mapbox.js/plugins/leaflet-label/v0.2.1/leaflet.label.css"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            addScriptRemote "https://cdn.firebase.com/js/client/2.2.1/firebase.js"
            addScriptRemote "https://api.tiles.mapbox.com/mapbox.js/v2.1.6/mapbox.js"
            addScriptRemote "https://api.tiles.mapbox.com/mapbox.js/plugins/leaflet-label/v0.2.1/leaflet.label.js"
            toWidget $(juliusFile "templates/julius/geolocalizacao.julius") 
            toWidget $(luciusFile "templates/lucius/principal.lucius") 
            toWidget $(cassiusFile "templates/cassius/funcionario.cassius")
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
            toWidget $(whamletFile "templates/whamlet/funcionario.hamlet") 
        --Se não houver (é Nothing):
        Nothing -> redirect ErroR
  
----------------Páginas do Site - Home, Quem Somos, Serviços, Contato-----------------------

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
            setTitle "Sauípe Express|Home"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css 
            --addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
            toWidget $(luciusFile "templates/lucius/principal.lucius") 
            toWidget $(whamletFile "templates/whamlet/home.hamlet") 

getQuemSomosR :: Handler Html
getQuemSomosR = defaultLayout $ do
        setTitle "Sauípe Express|QuemSomos"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        --addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/contato.cassius") 
        toWidget $(whamletFile "templates/whamlet/quemsomos.hamlet")

getServicosR :: Handler Html
getServicosR = defaultLayout $ do
        setTitle "Sauípe Express|Serviços"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        --addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/head.hamlet")
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(whamletFile "templates/whamlet/servicos.hamlet")

getContatoR :: Handler Html  
getContatoR = do  
        defaultLayout $ do
        setTitle "Sauípe Express|Contato" 
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_fontawesomemin_css
        addStylesheet $ StaticR css_main_css
        --addStylesheet $ StaticR css_principal_css
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
        toWidgetHead $(hamletFile "templates/hamlet/headcontato.hamlet")
        toWidget $(luciusFile "templates/lucius/principal.lucius") 
        toWidget $(cassiusFile "templates/cassius/contato.cassius")
        toWidget $(whamletFile "templates/whamlet/contato.hamlet")
         
--------------------- Ações ----------------------

-- Pagina de Erro 
getErroR :: Handler Html
getErroR = defaultLayout $ do  
            setTitle "Sauípe Express"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            --addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "templates/hamlet/headhome.hamlet")
            toWidget $(luciusFile "templates/lucius/principal.lucius") 
            toWidget $(whamletFile "templates/whamlet/error.hamlet") 

-- Pagina de Sucesso 
getSucessoR :: Handler Html
getSucessoR = defaultLayout $ do  
            setTitle "Sauípe Express"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_fontawesomemin_css
            addStylesheet $ StaticR css_main_css
            --addStylesheet $ StaticR css_principal_css
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
            toWidgetHead $(hamletFile "templates/hamlet/headadmin.hamlet")
            toWidget $(luciusFile "templates/lucius/principal.lucius") 
            toWidget $(whamletFile "templates/whamlet/sucesso.hamlet") 

 