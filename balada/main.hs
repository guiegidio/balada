{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Rotas
import Yesod
import Foundation
import Handlers
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql
import Prelude
import Yesod.Static
--Chamada para conexao no banco de dados
connStr = "dbname=d5ll0n50qdhk3a host=ec2-50-19-252-72.compute-1.amazonaws.com user=cdgruoqzghdfzj password=hKNYcy4w2v5mvzA0Q9jhgxAhc6 port=5432"

--main = warpEnv Balada
main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       t@(Static settings) <- static "static"
       warp 8080 (Balada t pool)