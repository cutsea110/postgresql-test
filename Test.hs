{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, FlexibleContexts #-}
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.TH (mkPersist, persist, share, mkMigrate, sqlSettings)
import Database.Persist.GenericSql (runSqlConn, runMigration)
import Database.Persist.GenericSql.Raw (withStmt)
import Data.Text (Text, pack)
import Database.Persist
import Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Post
  content Text
|]

connStr :: ConnectionString
connStr = "host=localhost user=cutsea110 password=cutsea110 port=5432 dbname=Test"

main :: IO ()
main = withPostgresqlConn connStr $ runSqlConn $ do
  runMigration migrateAll
  txt <- liftIO $ fmap pack $ readFile "longText.txt"
  let sql = "INSERT INTO \"Post\" (content) values (?);SELECT * FROM \"Post\";"
  C.runResourceT ( withStmt sql [toPersistValue txt]
    C.$$ CL.mapM_ $ liftIO . print )
