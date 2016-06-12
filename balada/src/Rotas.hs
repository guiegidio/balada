{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Rotas where

import Yesod
 
pRoutes = [parseRoutes|
/ HomeR GET
  /cadastro CadastroR GET 
  /administrador AdministradoR GET 
  /perfil/#PessoasId PerfilR GET
  /login LoginR GET POST
  /erro ErroR GET
  /about AboutR GET
  /admin AdminR GET 
  /logout LogoutR GET
  /cadpessoa CadpessoaR GET POST
  /cadestab CadestabR GET POST
  /cadcategoria CadcategoriaR GET POST
  /cadsubcateg CadsubcategR GET POST
  /caddia CaddiaR GET POST
  /cadfaixapreco CadfaixaprecoR GET POST
  
|]