--postgres database Crimson
{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
             
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Balada = Balada{connPool::ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Users json
   nome Text
   login Text
   senha Text
   deriving Show
   
Bar json
   nome Text
   endereco Text
|]

mkYesod "Balada" [parseRoutes|
  / HomeR GET
  /cadastro CadastroR GET POST
  /perfil/#UsersId PerfilR GET
  /login LoginR GET POST
  /erro ErroR GET
  /about AboutR GET
|]

instance Yesod Balada where

--Detalhamento da  Conexao 

instance YesodPersist Balada where
    type YesodPersistBackend Balada = SqlBackend
    runDB f = do
      master <- getYesod
      let pool = connPool master
      runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Balada FormMessage where
    renderMessage _ _ = defaultFormMessage
    
formUser :: Form Users
formUser = renderDivs $ Users <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing
           
formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

getHomeR = defaultLayout [whamlet|
    <h1>Welcome to the Party!</h1>
    <background-color: red>
    <a href=@{LoginR}>LOGIN
    <a href=@{CadastroR}>CADASTRE-SE
    <a href=@{AboutR}>Saiba Mais
  |]
  
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
               --FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
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

getAboutR = defaultLayout [whamlet|
    Essa pagina foi criada para acelerar o processo de decidir baladas!!!
    <a href=@{HomeR}>Back Home
|]

--Chamada para conexao no banco de dados
connStr = "dbname=d5ll0n50qdhk3a host=ec2-50-19-252-72.compute-1.amazonaws.com user=cdgruoqzghdfzj password=hKNYcy4w2v5mvzA0Q9jhgxAhc6 port=5432"

--main = warpEnv Balada
main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Balada pool)