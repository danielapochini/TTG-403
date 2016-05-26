{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Routes where

import Yesod
 
pRoutes = [parseRoutes|
   / HomeR GET
   /admin AdminR GET
   /cadastro UsuarioR GET POST
   /contato ContatoR GET
   /erro ErroR GET
   /login LoginR GET POST
   /logout LogoutR GET
   /perfil/#UsuariosId PerfilR GET
   /quemsomos QuemSomosR GET
   /servicos ServicosR GET
   /sucesso SucessoR GET
   /static StaticR Static getStatic
|]