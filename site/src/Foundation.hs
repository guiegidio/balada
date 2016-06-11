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
Users json
   nome Text
   login Text
   senha Text
   deriving Show
   
Bar json
   nome Text
   endereco Text
   
Evento json
    nome Text
    dia Text
    descricao Text
    barid BarId
    
Cadastro_pessoa json
    nm_pessoa Text
    dc_senha Text
    nm_cidade Text
    dc_email Text
    nu_ddd Text
    nu_telefone Text
    id_delecao Text
    
Interesse_pessoa
    cd_categoria Text
    cd_subcategoria Text
    id_delecao Text
    
Categoria_estabelecimento json
    nm_categoria Text

Subcategoria_estabelecimento json
    nm_subcategoria Text
    cd_categoria Text

Estabelecimento json
    nm_estabelecimento Text
    dc_email Text
    nu_ddd Text
    nu_telefone Text
    nm_cidade Text
    id_delecao Text

Categoria_estab json
    cd_estabelecimento Text
    cd_categoria Text
    cd_subcategoria Text
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