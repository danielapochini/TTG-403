{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, EmptyDataDecls, ViewPatterns #-}
 
module Handlers where
import Routes
import Yesod
import Utils
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
import Tipo
import Yesod.Form.Jquery
import Yesod.Static
import Network.Mail.Mime
import Database.Persist.Postgresql
-- import Network.Mail.Mime (renderEmail)

mkYesodDispatch "SauipeExpress" pRoutes
 
-- formulário de Contato
contatoForm :: Form Email
contatoForm = renderDivs $ Email <$>   
       areq textField (fieldSettingsLabel MsgTxtNome) Nothing <*>  
       areq textField (fieldSettingsLabel MsgTxtEmail) Nothing <*> 
       areq textField (fieldSettingsLabel MsgContato5) Nothing <*>
       areq textareaField (fieldSettingsLabel MsgMensagem) Nothing  
       
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
       areq (selectField $ optionsPairs [(MsgForm3, Administrador),(MsgForm2, Funcionario)]) (fieldSettingsLabel MsgForm4) Nothing

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
       entidades <- runDB $ selectList [UsuariosTipo ==. Funcionario] [Asc UsuariosNome]
       optionsPairs $ fmap (\ent -> (usuariosNome $ entityVal ent, entityKey ent)) entidades  
--------------------- METODOS POST -----------------------------

postCadClienteR :: Handler Html
postCadClienteR = do
           ((result, _), _) <- runFormPost clienteForm
           case result of 
               FormSuccess cli -> (runDB $ insert cli) >> redirect SucessoR
               _ -> redirect ErroR               
              
postContatoR :: Handler Html
postContatoR = do
           ((result, _), _) <- runFormPost contatoForm
           case result of 
               FormSuccess contato -> (liftIO $ enviarEmail $ estruturaEmail contato) >> redirect Sucesso2R
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
                FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsuariosLogin ==. login, UsuariosSenha ==. senha] []
                   case user of  
                       Nothing -> redirect LoginR 
                       Just (Entity pid (Usuarios nome login senha Administrador)) ->  setSession "_ID" (pack $ show $ Administrador) >> redirect AdminR
                       Just (Entity pid (Usuarios nome login senha Funcionario)) ->  setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (FuncionarioR)
                _ -> redirect ErroR  
                
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
            padmWidget $(whamletFile "templates/whamlet/admin.hamlet") >> geoWidget 
            toWidget $(juliusFile "templates/julius/geoadmin.julius") 
            toWidget $(cassiusFile "templates/cassius/admin.cassius") 

--Página de Cadastro de Filial
getCadFilialR :: Handler Html
getCadFilialR = do  
        (widget, enctype) <- generateFormPost filialForm
        defaultLayout $ do 
        setTitle "Sauípe Express| Cadastro Filial Empresa" 
        padmWidget $(whamletFile "templates/whamlet/cadastro/cadfilial.hamlet") 
        >> cadWidget
        
getListFilialR :: Handler Html
getListFilialR = do
        filial <- runDB $ selectList [] [Asc FilialNome]
        defaultLayout $ do 
        setTitle "Sauípe Express|Lista de Filial"   
        padmWidget $(whamletFile "templates/whamlet/listagem/listfilial.hamlet")
        >> listWidget
         
        
--Página de Cadastro de Cliente
getCadClienteR :: Handler Html
getCadClienteR = do  
        (widget, enctype) <- generateFormPost clienteForm
        defaultLayout $ do 
        setTitle "Sauípe Express| Cadastro Cliente" 
        padmWidget $(whamletFile "templates/whamlet/cadastro/cadcliente.hamlet")    
        >> cadWidget

getListClienteR :: Handler Html        
getListClienteR = do
        cliente <- runDB $ selectList [] [Asc ClienteNome]
        defaultLayout $ do 
        setTitle "Sauípe Express|Lista de Filial" 
        padmWidget $(whamletFile "templates/whamlet/listagem/listcliente.hamlet")      
        >> listWidget
        
--Página de Cadastro de Entrega 
getCadEntregaR :: Handler Html
getCadEntregaR = do  
        (widget, enctype) <- generateFormPost entregaForm
        defaultLayout $ do 
        setTitle "Sauípe Express| Cadastro Entrega" 
        padmWidget $(whamletFile "templates/whamlet/cadastro/cadentrega.hamlet")    
        >> cadWidget

getListEntregaR :: Handler Html        
getListEntregaR = do 
        entregas <- runDB $ (rawSql "SELECT ??, ??, ?? FROM entrega INNER JOIN cliente ON entrega.cliente_id=cliente.id INNER JOIN usuarios ON entrega.funcionario_id=usuarios.id" [])::Handler [(Entity Entrega, Entity Cliente, Entity Usuarios)] 
        defaultLayout $ do 
        setTitle "Sauípe Express|Lista Entregas" 
        padmWidget $(whamletFile "templates/whamlet/listagem/listentrega.hamlet") 
        >> listWidget

-- Página Cadastro de Funcionário 
getCadUsuarioR :: Handler Html
getCadUsuarioR = do  
        (widget, enctype) <- generateFormPost usuarioForm
        defaultLayout $ do 
        setTitle "Sauípe Express| Cadastro Funcionário" 
        padmWidget $(whamletFile "templates/whamlet/cadastro/cadusuario.hamlet") 
        >> cadWidget

-- Lista Funcionário Cadastrado 
getListUsuarioR :: Handler Html
getListUsuarioR = do
        listaP <- runDB $ selectList [] [Asc UsuariosNome]
        defaultLayout $ do 
        setTitle "Sauípe Express|Lista" 
        padmWidget $(whamletFile "templates/whamlet/listagem/listusuario.hamlet") 
        >> listWidget
 
---------------- Login, Logout -----------------------

-- Página Login 
getLoginR :: Handler Html
getLoginR = do
        (widget, enctype) <- generateFormPost loginForm
        defaultLayout $ do 
        setTitle "Sauípe Express|Login" 
        padmWidget $(whamletFile "templates/whamlet/login.hamlet")    
        toWidget $(cassiusFile "templates/cassius/form.cassius") 
        
 
-- Pagina de Logout 
getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout $ do
            setTitle "Sauípe Express" 
            padmWidget $(whamletFile "templates/whamlet/logout.hamlet") 
            toWidgetHead $(hamletFile "templates/hamlet/headhome.hamlet") 
        
----------- PERFIL USUARIO -------------      

getPerfilR :: UsuariosId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do 
            setTitle "Sauípe Express|Funcionário" 
            padmWidget $(whamletFile "templates/whamlet/perfil/perfil.hamlet") 
            >> perfWidget
    
----------- PERFIL Cliente -------------      

getPerfilClienteR :: ClienteId -> Handler Html
getPerfilClienteR uid = do
      cliente <- runDB $ get404 uid
      defaultLayout $ do 
            setTitle "Sauípe Express|Cliente" 
            padmWidget $(whamletFile "templates/whamlet/perfil/cliente.hamlet")   
            >> perfWidget

----------- PERFIL Filial -------------      

getPerfilFilialR :: FilialId -> Handler Html
getPerfilFilialR uid = do
      filial <- runDB $ get404 uid
      defaultLayout $ do 
            setTitle "Sauípe Express|Filial"
            padmWidget $(whamletFile "templates/whamlet/perfil/filial.hamlet")  
            >> perfWidget
        
--------------------------- FUNCIONARIO  --------------------
getFuncionarioR :: Handler Html
getFuncionarioR = do
     mu <- lookupSession "_ID"
     case mu of 
        Just sess -> do 
            uid <- return (toSqlKey $ read $ unpack sess) :: Handler (Key Usuarios)
            user <- runDB $ get404 uid
            defaultLayout $ do 
            setTitle "Sauípe Express|Funcionário"  
            padmWidget $(whamletFile "templates/whamlet/funcionario.hamlet") >> geoWidget
            toWidget $(cassiusFile "templates/cassius/funcionario.cassius") 
            toWidget $(juliusFile "templates/julius/geolocalizacao.julius")
            
        Nothing -> redirect ErroR
  
----------------Páginas do Site - Home, Quem Somos, Serviços, Contato-----------------------

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
        setTitle "Sauípe Express|Home" 
        customWidget $(whamletFile "templates/whamlet/home.hamlet") 

getQuemSomosR :: Handler Html
getQuemSomosR = defaultLayout $ do
        setTitle "Sauípe Express|QuemSomos" 
        customWidget $(whamletFile "templates/whamlet/quemsomos.hamlet")
        toWidget $(cassiusFile "templates/cassius/contato.cassius") 

getServicosR :: Handler Html
getServicosR = defaultLayout $ do
        setTitle "Sauípe Express|Serviços" 
        customWidget $(whamletFile "templates/whamlet/servicos.hamlet")

getContatoR :: Handler Html  
getContatoR = do  
        (widget, enctype) <- generateFormPost contatoForm
        defaultLayout $ do
        setTitle "Sauípe Express|Contato"   
        customWidget $(whamletFile "templates/whamlet/contato.hamlet")
        toWidget $(cassiusFile "templates/cassius/contato.cassius")
         
--------------------- Ações ----------------------

-- Pagina de Erro 
getErroR :: Handler Html
getErroR = defaultLayout $ do  
        setTitle "Sauípe Express|Erro!" 
        customWidget $(whamletFile "templates/whamlet/error.hamlet")  
        toWidgetHead $(hamletFile "templates/hamlet/headhome.hamlet")

-- Pagina de Sucesso 
getSucessoR :: Handler Html
getSucessoR = defaultLayout $ do  
        setTitle "Sauípe Express"  
        padmWidget $(whamletFile "templates/whamlet/sucesso.hamlet") 
        toWidgetHead $(hamletFile "templates/hamlet/headadmin.hamlet") 
        
 -- Pagina de Sucesso Email       
getSucesso2R :: Handler Html
getSucesso2R = defaultLayout $ do  
        setTitle "Sauípe Express"  
        padmWidget $(whamletFile "templates/whamlet/sucesso.hamlet") 
        toWidgetHead $(hamletFile "templates/hamlet/headcontato.hamlet") 

 
 