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

--FORMULARIOS
formPessoa :: Form Pessoas
formPessoa = renderDivs $ Pessoas <$>
    areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="CPF",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","11")]} Nothing<*>
    areq textField "Nome: " Nothing <*>
    areq textField "Login " Nothing <*>
    areq passwordField "Senha: " Nothing <*>
    areq textField "Cidade: " Nothing <*>
    areq textField "E-mail: " Nothing <*>
    areq textField "DDD: " Nothing <*>
    areq textField "Celular: " Nothing

formEstabelecimento :: Form Estabelecimento
formEstabelecimento = renderDivs $ Estabelecimento <$>
    areq textField "Nome: " Nothing <*>
    areq textField "Cidade: " Nothing <*>
    areq textField "Email: " Nothing <*>
    areq textField "DDD: " Nothing <*>
    areq textField "Telefone: " Nothing 

--Form T_categoria_estab
formCategoria :: Form Categoria_estab
formCategoria = renderDivs $ Categoria_estab <$>
    areq textField "Categoria: " Nothing 

--Form T_sub_categ_estab
formSubcategoria :: Form Sub_categ_estab
formSubcategoria = renderDivs $ Sub_categ_estab <$>
    areq textField "Categoria" Nothing <*>
    areq textField "Subcategoria: " Nothing

--Form T_faixa_preco    
formFaixapreco :: Form Faixa_preco
formFaixapreco =  renderDivs $ Faixa_preco <$>
    areq textField "Nome: " Nothing <*>
    areq textField "Valor Inicial: " Nothing <*>
    areq textField "Valor Final: " Nothing 
    
formDia :: Form Dia_evento
formDia =  renderDivs $ Dia_evento <$>
    areq textField "Dia: " Nothing 

--Form T_F_interesse_pessoa

--Form Login
formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
    areq textField "Email: " Nothing <*>
    areq passwordField "Senha: " Nothing
    
--Lista Drop Down
-- testar o funfar
estabs = do 
        entidades <- runDB $ selectList [] [Asc EstabelecimentoNome_estab]
        optionsPairs $ fmap(\ent -> (estabelecimentoNome_estab $ entityVal ent, entityKey ent)) entidades

categs = do 
        entidades <- runDB $ selectList [] [Asc Categoria_estabNome_categoria]
        optionsPairs $ fmap(\ent -> (categoria_estabNome_categoria $ entityVal ent, entityKey ent)) entidades
    
listaSubcategoria = do 
                   entidades <- runDB $ selectList [] [Asc Sub_categ_estabNome_sub_categ] 
                   optionsPairs $ fmap(\ent ->(sub_categ_estabNome_sub_categ $ entityVal ent, entityKey ent)) entidades

listaClassificacao = do
                    entidades <- runDB $ selectList [] [Asc Classificacao_estabNome_classificacao] 
                    optionsPairs $ fmap(\ent ->(classificacao_estabNome_classificacao $ entityVal ent, entityKey ent)) entidades
    
listaFaixapreco = do
                 entidades <- runDB $ selectList [] [Asc Faixa_precoNome_faixa_preco] 
                 optionsPairs $ fmap(\ent ->(faixa_precoNome_faixa_preco $ entityVal ent, entityKey ent)) entidades
    
listaDia = do
          entidades <- runDB $ selectList [] [Asc Dia_eventoDc_dia] 
          optionsPairs $ fmap(\ent ->(dia_eventoDc_dia $ entityVal ent, entityKey ent)) entidades

--PAGINAS
getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
            toWidget $(whamletFile "Hamlets/home.hamlet")

--getCadastroR :: Handler Html
--getCadastroR = defaultLayout $ do 
--                toWidget $(whamletFile "Hamlets/cadastro.hamlet")

--getAdministradorR :: Handler Html
--getAdministradorR = defaultLayout $ do 
  --                  toWidget $(whamletFile "Hamlets/administrador.hamlet")

--Alcool na mesa
getCadpessoaR :: Handler Html
getCadpessoaR = do 
    (widget, enctype) <-generateFormPost formPessoa
    defaultLayout [whamlet|
        <center> <form method=post enctype=#{enctype} action=@{CadpessoaR}>
        ^{widget}
        <input type="submit" value="Enviar">
        <h1> Cadastro Completo
|]

getCadestabR :: Handler Html
getCadestabR = do 
    (widget, enctype) <-generateFormPost formEstabelecimento
    defaultLayout [whamlet|
        <center> <form method=post enctype=#{enctype} action=@{CadestabR}>
        ^{widget}
        <input type="submit" value="Enviar">
        <h1> Cadastro de estabelecimento Completo
|]
    
getCadcategoriaR :: Handler Html
getCadcategoriaR = do 
    (widget, enctype) <-generateFormPost formCategoria
    defaultLayout [whamlet|
        <center> <form method=post enctype=#{enctype} action=@{CadcategoriaR}>
        ^{widget}
        <input type="submit" value="Enviar">
        <h1> Cadastro de categoria Completo
|]

getCadsubcategR :: Handler Html
getCadsubcategR = do
    (widget, enctype) <-generateFormPost formSubcategoria
    defaultLayout [whamlet|
        <center> <form method=post enctype=#{enctype} action=@{CadsubcategR}>
        ^{widget}
        <input type="submit" value="Enviar">
        <h1> Cadastro de subcategoria Completo
|]

getCaddiaR :: Handler Html 
getCaddiaR = do
    (widget, enctype) <-generateFormPost formDia
    defaultLayout [whamlet|
        <center> <form method=post enctype=#{enctype} action=@{CaddiaR}>
        ^{widget}
        <input type="submit" value="Enviar">
        <h1> Cadastro de dia evento Completo
|]

getCadfaixaprecoR :: Handler Html    
getCadfaixaprecoR = do
    (widget, enctype) <-generateFormPost formFaixapreco
    defaultLayout [whamlet|
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
        FormSuccess pessoa -> (runDB $ insert pessoa) >>= \cpf_pessoa -> redirect (CadpessoaR)
        _ -> redirect ErroR
        
postCadestabR :: Handler Html
postCadestabR = do
    ((result, _),_) <- runFormPost formEstabelecimento
    case result of
        FormSuccess estab -> (runDB $ insert estab) >>= \cd_estabelecimento -> redirect (CadestabR)
        _ -> redirect ErroR

postCadcategoriaR :: Handler Html
postCadcategoriaR = do
    ((result, _),_) <- runFormPost formCategoria
    case result of
        FormSuccess categoria -> (runDB $ insert categoria) >>= \cd_categoria_estab -> redirect (CadcategoriaR)
        _ -> redirect ErroR
        
postCadsubcategR :: Handler Html
postCadsubcategR = do
    ((result, _),_) <- runFormPost formSubcategoria
    case result of
        FormSuccess subcategoria -> (runDB $ insert subcategoria) >>= \cd_sub_categ_estab -> redirect (CadsubcategR)
        _ -> redirect ErroR
        
postCadfaixaprecoR :: Handler Html
postCadfaixaprecoR = do
    ((result, _),_) <- runFormPost formFaixapreco
    case result of
        FormSuccess faixapreco -> (runDB $ insert faixapreco) >>= \cd_faixa_preco -> redirect (CadfaixaprecoR)
        _ -> redirect ErroR
        
postCaddiaR :: Handler Html
postCaddiaR = do
    ((result, _),_) <- runFormPost formDia
    case result of
        FormSuccess dia -> (runDB $ insert dia) >>= \cd_dia -> redirect (CadfaixaprecoR)
        _ -> redirect ErroR



--Limbo Mental
getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
    <h1> Bem-vindo meu Rei!
|]


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
                   user <- runDB $ selectFirst [PessoasEmail ==. login, PessoasSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)

getPerfilR :: PessoasId -> Handler Html
getPerfilR uid = do
      pessoas <- runDB $ get404 uid
      defaultLayout [whamlet|
          <p><b> Pagina de #{pessoasNome pessoas}
          <p><b> Login: #{pessoasLogin pessoas}
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
