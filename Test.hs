{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, FlexibleContexts #-}
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.TH (mkPersist, persist, share, mkMigrate, sqlSettings)
import Database.Persist.GenericSql (runSqlConn, runMigration)
import Data.Text (Text, pack)
import Database.Persist
import Control.Monad.IO.Class (liftIO)

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
  pid <- insert Post { postContent=txt }
  liftIO $ print pid
  update pid [ PostContent =. txt ]
  mp <- get pid
  case mp of
    Nothing -> liftIO $ print "failed."
    Just p -> liftIO $ print $ postContent p
