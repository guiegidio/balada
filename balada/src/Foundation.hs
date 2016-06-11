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

T_cadastro_pessoa json
    cd_cpf_pessoa Text
    nm_pessoa Text
    dc_senha Text
    nm_cidade Text
    dc_email Text
    nu_ddd Text
    nu_celular Text
    cd_delecao Text
    
T_estabelecimento json
    cd_estabelecimento Text
    nm_estabelcimento Text
    nm_cidade_estab Text
    dc_email_estab Text
    nu_ddd_estab Text
    nu_telefone_estab Text
    cd_delecao Text
    
T_categoria_estab json
    cd_categoria_estab Text
    nm_categoria_estab Text
    
T_sub_categ_estab json
    nm_sub_categ_estab Text
    cd_categoria_estab Text

T_faixa_preco json
    nm_faixa_preco Text
    qt_valor_minimo Text
    qt_valor_maximo Text

T_classificacao_estab json
    nm_classificacao_estab Text
    qt_classificacao_estab Text
    
T_dia_evento json
    cd_dia Text
    dc_dia Text
    
T_F_interesse_pessoa json
    cd_cpf_pessoa Text
    cd_categoria_estab Text
    cd_sub_categ_estab Text
    cd_faixa_preco Text
    cd_classificacao_estab Text
    cd_delecao Text
    
T_F_estabelecimento json
    cd_estabelecimento T_estabelecimentoId
    cd_categoria_estab T_categoria_estabId
    cd_sub_categ_estab T_sub_categ_estabId
    cd_faixa_preco Text
    cd_dia Text
    cd_classificacao_estab Text

|]


mkYesodData "Balada" pRoutes

--staticFile "static"

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