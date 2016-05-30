{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Routes where

import Yesod
 
pRoutes = [parseRoutes|
   / HomeR GET
   /admin AdminR GET
   /admin/cad/funcionario/ CadFuncionarioR GET POST
   /admin/list/funcionario/ ListFuncionarioR GET
   /contato ContatoR GET
   /erro ErroR GET
   /funcionario FuncionarioR GET
   /login LoginR GET POST
   /logout LogoutR GET
   /perfil/#UsuariosId PerfilR GET POST
   /quemsomos QuemSomosR GET
   /servicos ServicosR GET
   /static StaticR Static getStatic
   /sucesso SucessoR GET
|]

-- admin/cadastro/cliente CadClienteR GET POST
