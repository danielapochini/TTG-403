{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

{-- tipo Pagina com um data Constructor Pagina --} 
data Pagina = Pagina{connPool :: ConnectionPool}


{-- 
 lista todas os usuarios no banco a partir do nome
getUsuarioR :: Handler ()
getUsuarioR = do
    allUsuarios <- runDB $ selectList [] [Asc UsuariosNome]
    sendResponse (object [pack "data" .= fmap toJSON allUsuarios])

 cria usuario no banco 
postUsuarioR :: Handler ()
postUsuarioR = do
    usuarios <- requireJsonBody :: Handler Usuarios
    runDB $ insert usuarios
    sendResponse (object [pack "resp" .= pack "CRIADO"])

-- se o usuario nao exisitr, dá erro 404
getActionR :: UsuariosId m a -> a Handler ()
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
 
 
{--
 o tipo Pagina é uma instancia da classe Yesod, definida na biblioteca Yesod. 
 Yesod significa fundação em Hebreu, entao Pagina forma a fundação de nosso website.
--}
instance Yesod Pagina
-- 15, 16, 17, 21 e 22

--tabela
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuarios json
   nome Text
   senha Text
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
{-- caminho da rota, nome da rota (Data Constructor), metodo de requisição --}
/cadastro UsuarioR GET POST
/cadastro/action/#UsuariosId ActionR GET PUT DELETE 
-- /cadastro/buscar/#UsuariosNome BuscaR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------

-- lista todas os usuarios no banco a partir do nome
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


-- se o usuario nao exisitr, dá erro 404
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
    sendResponse (object [pack "resp" .= pack "DELETADO")
    
       

    
{- 
mkYesod "Pagina" [parseRoutes|
/cadastro UserR GET POST
/cadastro/action/#ClientesId ActionR GET PUT DELETE
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------
getUserR :: Handler ()
getUserR = do
    allClientes <- runDB $ selectList [] [Asc ClientesNome]
    sendResponse (object [pack "data" .= fmap toJSON allClientes])
    
postUserR :: Handler ()
postUserR = do
    clientes <- requireJsonBody :: Handler Clientes
    runDB $ insert clientes
    sendResponse (object [pack "resp" .= pack "CREATED"])

getActionR :: ClientesId -> Handler ()
getActionR pid = do
    cli <- runDB $ get404 pid
    sendResponse $ toJSON cli

deleteActionR :: ClientesId -> Handler ()
deleteActionR pid = do
    runDB $ delete pid
    sendResponse (object [pack "resp" .= pack "DELETED"])
    
putActionR :: ClientesId -> Handler ()
putActionR pid = do
    cli <- requireJsonBody :: Handler Clientes
    runDB $ update pid [ClientesNome =. clientesNome cli]
    
    
    getBuscaR :: UsuariosNome -> Handler ()
getBuscaR text = do
    allUsuarios <- runDB $ selectList [text] [Asc UsuariosNome]
    sendResponse (object [pack "data" .= toJSON allUsuarios])
-}
connStr = "dbname=dc4os3bfc24nle host=ec2-23-21-165-201.compute-1.amazonaws.com user=mbyfpmgifqzsml password=tx6KvtIZYlmna5zW4D4ISJ_FNU"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool) --porta que sera executada


{- comandos:
cd web2
    stack build

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