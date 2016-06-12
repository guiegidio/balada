{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Rotas
import Yesod
import Data.Text
import Prelude
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Balada = Balada{connPool::ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Pessoas json
    cpf_pessoa Text sqltype=varchar(11)
    nome Text
    senha Text
    cidade Text
    email Text
    ddd Text
    celular Text
    delecao Text
    deriving Show
    
Estabelecimento json
    nome_estab Text
    cidade_estab Text
    email_estab Text
    ddd_estab Text
    telefone_estab Text
    delecao_estab Text
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

mkYesodData "Balada" pRoutes

instance YesodPersist Balada where
    type YesodPersistBackend Balada = SqlBackend
    runDB f = do
      master <- getYesod
      let pool = connPool master
      runSqlPool f pool
      
instance Yesod Balada where

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Balada FormMessage where
    renderMessage _ _ = defaultFormMessage