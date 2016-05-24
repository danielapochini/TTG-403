{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Routes 
import Yesod
import Foundation
import Handlers
import Yesod.Static
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

-- conexão com o BD          
connStr = "dbname=dc4os3bfc24nle host=ec2-23-21-165-201.compute-1.amazonaws.com user=mbyfpmgifqzsml password=tx6KvtIZYlmna5zW4D4ISJ_FNU"

main :: IO ()
main = do
       s@(Static settings) <- static "static"    
       runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (SauipeExpress s pool)