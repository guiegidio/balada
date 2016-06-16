{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Rotas
import Yesod
import Data.Text
import Data.Maybe
import Prelude
import Yesod.Static
import Text.Lucius
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Balada = Balada{getStatic :: Static, connPool::ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Pessoas json
    cpf_pessoa Text sqltype=varchar(11)
    nome Text
    login Text
    senha Text
    cidade Text
    email Text
    ddd Text
    celular Text
    --delecao Text
    deriving Show
    
Estabelecimento json
    nome_estab Text
    end_estab Text
    cidade_estab Text
    email_estab Text
    ddd_estab Text
    telefone_estab Text
    categoria Categoria_estabId
    preco Faixa_precoId
    --delecao_estab Text
    deriving Show
    
Categoria_estab json
    nome_categoria Text
    deriving Show
    
Sub_categ_estab json
    nome_sub_categ Text
    cd_categoria_estab Text
    deriving Show
    
Faixa_preco json
    nome_faixa_preco Text
    valor_minimo Text sqltype=float
    valor_maximo Text sqltype=float
    deriving Show
    
Classificacao_estab json
    nome_classificacao Text
    qt_classificacao Text
    deriving Show
    
Dia_evento json
    dc_dia Text
    deriving Show
    
Interesse json
    cpf_pessoa Text sqltype=varchar(11)
    cd_categoria_estab Categoria_estabId
    cd_sub_categ_estab Sub_categ_estabId
    cd_faixa_preco Faixa_precoId
    cd_classificacao_estab Classificacao_estabId
    cd_delecao Text
    deriving Show
    
T_F_estabelecimento json
    cd_estabelecimento EstabelecimentoId
    cd_categoria_estab Categoria_estabId
    cd_sub_categ_estab Sub_categ_estabId
    cd_faixa_preco Faixa_precoId
    cd_dia Dia_eventoId
    cd_classificacao_estab Classificacao_estabId
    cd_delecao Text
    deriving Show
|]

staticFiles "static"

mkYesodData "Balada" pRoutes

instance Yesod Balada where
authRoute _ = Just LoginR

isAuthorized LoginR _ = return Authorized
isAuthorized ErroR _ = return Authorized
isAuthorized HomeR _ = return Authorized
isAuthorized CadpessoaR _ = return Authorized
isAuthorized CadestabR _ = return Authorized
isAuthorized CadcategoriaR _ = isAdmin
isAuthorized CadsubcategR _ = isAdmin
isAuthorized CaddiaR _ = isAdmin
isAuthorized CadfaixaprecoR _ = isAdmin
isAuthorized ListaestabsR _ = isUser
isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Necessário Login de Administrador"


instance YesodPersist Balada where
    type YesodPersistBackend Balada = SqlBackend
    runDB f = do
      master <- getYesod
      let pool = connPool master
      runSqlPool f pool
      


type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Balada FormMessage where
    renderMessage _ _ = defaultFormMessage