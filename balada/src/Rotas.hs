{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Rotas where

import Yesod
 
pRoutes = [parseRoutes|
/ HomeR GET
  /cadastro CadastroR GET POST
  /perfil/#T_pessoasId PerfilR GET
  /login LoginR GET POST
  /erro ErroR GET
  /about AboutR GET
  /admin AdminR GET 
  /logout LogoutR GET
|]