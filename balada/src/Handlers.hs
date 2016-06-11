{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Rotas
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Text.Cassius

import Database.Persist.Postgresql

mkYesodDispatch "Balada" pRoutes

--Forms

formPessoa :: Form T_cadastro_pessoa
    areq textField "CPF: " Nothing <*>
    areq textField "Nome: " Nothing <*>
    areq textField "Senha: " Nothing <*>
    areq textField "Cidade: " Nothing <*>
    areq textField "E-mail: " Nothing <*>
    areq textField "DDD: " Nothing <*>
    areq textField "Celular: " Nothing <*>

formEstabelecimento :: Form T_estabelecimento
    areq textField "Nome: " Nothing <*>
    areq textField "Cidade: " Nothing <*>
    areq textField "Email: " Nothing <*>
    areq textField "DDD: " Nothing <*>
    areq textField "Telefone: " Nothing <*>

formCategoria :: Form T_categoria_estab
    areq textField "Categoria: " Nothing <*>
    
formSubcategoria :: Form T_sub_categ_estab
    areq (selectField listaCategoria) "Categoria" Nothing <*>
    areq textField "Subcategoria: " Nothing <*>
    
formFaixapreco :: Form T_faixa_preco
    areq textField "Nome: " Nothing <*>
    areq textField "Valor Inicial: " Nothing <*>
    areq textField "Valor Final: " Nothing <*

formInteresse :: Form T_F_interesse_pessoa
    areq (selectField listaCategoria) "Categoria" Nothing <*>
    areq (selectField listaSubcategoria) "Subcategoria" Nothing <*>
    areq (selectField listaFaixapreco) "Faixa Preco" Nothing <*>
    areq (selectField listaClassificacao) "Classificacao" Nothing <*>

formFestabelecimento :: Form T_F_estabelecimento
    areq (selectField listaCategoria) "Categoria" Nothing <*>
    areq (selectField listaSubcategoria) "Subcategoria" Nothing <*>
    areq (selectField listaFaixapreco) "Faixa Preco" Nothing <*>
    areq (selectField listaClassificacao) "Classificacao" Nothing <*>
    area (selectField listaDia) "Dia" Nothing <*>
    
--Lista Drop Down
-- testar o funfar
listaCategoria = do 
    categoria <- runDB $ selectList [] [Asc nm_categoria_estab] optionsPairs
    $ fmap(\x ->(nm_categoria_estab $ entityVal x, entityKey x)) categoria
    
listaSubcategoria = do 
    subcategoria <- runDB $ selectList [] [Asc nm_sub_categ_estab] optionsPairs
    $ fmap(\x ->(nm_categoria_estab $ entityVal x, entityKey x)) subcategoria

    
--PAGINAS 
getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
            toWidget $(whamletFile "Hamlets/home.hamlet")-- >> toWidget $(cassiusFile"Lucius/home.cassius")

getCadastroR :: Handler Html
getCadastroR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                <center> <form method=post enctype=#{enctype} action=@{CadastroR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
            <a href=@{HomeR}>Back Home
           |]

postCadastroR :: Handler Html
postCadastroR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR

getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{LoginR}>
                     ^{widget}
                     <input type="submit" value="Login">
                    <a href=@{HomeR}>Pagina Inicial
            |]

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)


getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout [whamlet|
          <p><b> Pagina de #{usersNome user}
          <p><b> Login: #{usersLogin user}
      |]

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
     <h1> Erro de cadastro
|]

getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> ADEUS!
     |]
     
getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
    <h1> Bem-vindo meu Rei!
|]

getAboutR = defaultLayout [whamlet|
    Essa pagina foi criada para acelerar o processo de decidir roles!!!
    <a href=@{HomeR}>Back Home
|]
