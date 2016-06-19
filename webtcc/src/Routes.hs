{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Routes where

import Yesod
 
pRoutes = [parseRoutes|
   / HomeR GET
   /admin AdminR GET
   /admin/cad/cliente/  CadClienteR GET POST
   /admin/cad/entrega/  CadEntregaR GET POST
   /admin/cad/filial/   CadFilialR GET POST
   /admin/cad/usuario/  CadUsuarioR GET POST
   /admin/list/cliente/ ListClienteR GET
   /admin/list/entrega/ ListEntregaR GET
   /admin/list/filial/  ListFilialR GET   
   /admin/list/usuario/ ListUsuarioR GET   
   /contato ContatoR GET POST
   /contato/sucesso Sucesso2R GET
   /erro ErroR GET
   /funcionario FuncionarioR GET
   /login LoginR GET POST
   /logout LogoutR GET
   /perfil/usuario/#UsuariosId PerfilR GET POST
   /perfil/filial/#FilialId PerfilFilialR GET POST
   /perfil/cliente/#ClienteId PerfilClienteR GET POST
   /quemsomos QuemSomosR GET
   /servicos ServicosR GET
   /static StaticR Static getStatic
   /sucesso SucessoR GET
|]

-- admin/cadastro/cliente CadClienteR GET POST
