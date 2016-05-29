{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Routes
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )
    
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

{--
Clientes json
    nome Text
    endereco Text
    cidade Text
    cep Text
    deriving Show
    
Administradores json
    nome text
    login Text
    senha Text

Clientes  
    cpf Int 
    nome Text  
    endereço Text
    nascimento Text
    email Text 
    sexo Text 
    ddd Text 
    telefone Text 
--}

staticFiles "static" -- pasta static 


mkMessage "SauipeExpress" "messages" "pt-BR" -- linguagem default

mkYesodData "SauipeExpress" pRoutes

--A função 'isAuthorized' determina os acessos por rota 
instance Yesod SauipeExpress where 

    errorHandler NotFound = redirect ErroR
    errorHandler other = defaultErrorHandler other

    authRoute _ = Just LoginR
    
    isAuthorized LoginR _     = return Authorized
    isAuthorized ErroR _      = return Authorized
    isAuthorized HomeR _      = return Authorized
    isAuthorized QuemSomosR _ = return Authorized
    isAuthorized ServicosR _  = return Authorized
    isAuthorized ContatoR _   = return Authorized 
    isAuthorized AdminR _     = isAdmin
    isAuthorized UsuarioR _   = isAdmin
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
       

-- renderiza a pagina no idioma de quem a acessa
instance RenderMessage SauipeExpress FormMessage where
    renderMessage _ _ = defaultFormMessage
    
type Form a = Html -> MForm Handler (FormResult a, Widget)
