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
formPessoa = renderDivs $ T_pessoas <$>
    areq textField "CPF: " Nothing <*>
    areq textField "Nome: " Nothing <*>
    areq textField "Senha: " Nothing <*>
    areq textField "Cidade: " Nothing <*>
    areq textField "E-mail: " Nothing <*>
    areq textField "DDD: " Nothing <*>
    areq textField "Celular: " Nothing <*>

formEstabelecimento :: FormEstabelecimento
formEstabelecimento = renderDivs $ T_estabelecimento <$>
    areq textField "Nome: " Nothing <*>
    areq textField "Cidade: " Nothing <*>
    areq textField "Email: " Nothing <*>
    areq textField "DDD: " Nothing <*>
    areq textField "Telefone: " Nothing <*>
    
--PAGINAS 
getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
            toWidget $(whamletFile "Hamlets/home.hamlet")-- >> toWidget $(cassiusFile"Lucius/home.cassius")

getCadastroR :: Handler Html
getCadastroR = defaultLayout $ do 
                toWidget $(whamletFile "Hamlets/cadastro.hamlet")

getAdministradorR :: Handler Html
getAdministradorR = defaultLayout $ do 
                    toWidget $(whamletFile "Hamlets/cadastro.hamlet")
                    


--Alcool na mesa
getCadpessoaR :: Handler Html
getCadpessoaR = do 
    (widget, enctype) <-generateFormPost formPessoa
    defaultLayout [whamlet |
        <center> <form method=post enctype=#{enctype} action=@{CadpessoaR}>
            ^{widget}
            <input type="submit" value="Enviar">
        <h1> Cadastro Completo
    |]

getCadestabR :: Handler Html
getCadestabR = do 
    (widget, enctype) <-generateFormPost formEstabelecimento
    defaultLayout [whamlet |
        <center> <form method=post enctype=#{enctype} action=@{CadestabR}>
            ^{widget}
            <input type="submit" value="Enviar">
        <h1> Cadastro de estabelecimento Completo
    |]
    
getCadcategoriaR :: Handler Html
getCadcategoriaR = do 
    (widget, enctype) <-generateFormPost formCategoria
    defaultLayout [whamlet |
        <center> <form method=post enctype=#{enctype} action=@{CadcategoriabR}>
            ^{widget}
            <input type="submit" value="Enviar">
        <h1> Cadastro de categoria Completo
    |]

getCadsubcategR :: Handler Html
getCadsubcategR = do
    (widget, enctype) <-generateFormPost formSubcategoria
    defaultLayout [whamlet |
        <center> <form method=post enctype=#{enctype} action=@{CadsubcategbR}>
            ^{widget}
            <input type="submit" value="Enviar">
        <h1> Cadastro de subcategoria Completo
    |]

getCaddiaR :: Handler Html 
getCaddiaR = do
    (widget, enctype) <-generateFormPost formDia
    defaultLayout [whamlet |
        <center> <form method=post enctype=#{enctype} action=@{CaddiaR}>
            ^{widget}
            <input type="submit" value="Enviar">
        <h1> Cadastro de dia evento Completo
    |]

getCadfaixaprecoR :: Handler Html    
getCadfaixaprecoR = do
    (widget, enctype) <-generateFormPost formFaixapreco
    defaultLayout [whamlet |
        <center> <form method=post enctype=#{enctype} action=@{CadfaixaprecoR}>
            ^{widget}
            <input type="submit" value="Enviar">
        <h1> Faixa de preco estabelecido
    |]

--alcool na garrafa
postCadpessoaR :: Handler Html
postCadpessoaR = do
    ((result, _),_) <- runFormPost formPessoa
    case result of
        FormSuccess pessoa -> (runDB $ insert pessoa) >> = cd_cpf_pessoa -> redirect (CadpessoaR cd_cpf_pessoa)
        _ -> redirect ErroR
        
postCadestabR :: Handler Html
postCadestabR = do
    ((result, _),_) <- runFormPost formEstabelecimento
    case result of
        FormSuccess estab -> (runDB $ insert estab) >> = cd_estabelecimento -> redirect (CadestabR cd_estabelecimento)
        _ -> redirect ErroR

postCadcategoriaR :: Handler Html
postCadcategoriaR = do
    ((result, _),_) <- runFormPost formCategoria
    case result of
        FormSuccess categoria -> (runDB $ insert categoria) >> = cd_categoria_estab -> redirect (CadcategoriabR cd_categoria_estab)
        _ -> redirect ErroR
        
postCadsubcategR :: Handler Html
postCadsubcategRaR = do
    ((result, _),_) <- runFormPost formSubcategoria
    case result of
        FormSuccess subcategoria -> (runDB $ insert subcategoria) >> = cd_sub_categ_estab -> redirect (CadsubcategbR cd_sub_categ_estab)
        _ -> redirect ErroR
        
postFaixaprecoR :: Handler Html
postFaixaprecoR = do
    ((result, _),_) <- runFormPost formFaixapreco
    case result of
        FormSuccess faixapreco -> (runDB $ insert faixapreco) >> = cd_faixa_preco -> redirect (CadfaixaprecoR cd_faixa_preco)
        _ -> redirect ErroR
    
postCadclassificacaoR :: Handler Html
postCadclassificacaoR = do
    ((result, _),_) <- runFormPost formClassificacao
    case result of
        FormSuccess classificacao -> (runDB $ insert classificacao) >> = cd_classificacao_estab -> redirect (CadclassificacaoR cd_classificacao_estab)
        _ -> redirect ErroR
    

    
    
    
--Limbo Mental
getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
    <h1> Bem-vindo meu Rei!
|]


getCadastrarR :: Handler Html
getCadastrarR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                <center> <form method=post enctype=#{enctype} action=@{CadastrarR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
            <a href=@{HomeR}>Back Home
           |]

postCadastrarR :: Handler Html
postCadastrarR = do
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
     

getAboutR = defaultLayout [whamlet|
    Essa pagina foi criada para acelerar o processo de decidir roles!!!
    <a href=@{HomeR}>Back Home
|]
