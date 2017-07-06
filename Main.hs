{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
import Foundation
import Application () -- for YesodDispatch instance
import Yesod
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql
import Yesod.Static

connStr :: ConnectionString
connStr = "dbname=dc92svd8320e58 host=ec2-54-221-226-148.compute-1.amazonaws.com user=ixrgpegxdbuhlb password=lvEN2G2U0KvfPq9aSRf1AT59KF port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       pastaStatic@(Static settings) <- static "static"
       warp 8080 (App pastaStatic pool)