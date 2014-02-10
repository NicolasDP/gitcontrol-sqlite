{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import System.GitControl

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import System.Posix.Env.ByteString
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
      [persistLowerCase|
Repo
    path BS.ByteString
    UniqueRepo path
    deriving Show
User
    ident BS.ByteString
    UniqueUser ident
    deriving Show
Mode
    prjId RepoId Eq
    personId UserId Eq
    priv String
    UniqueMode prjId personId
    deriving Show
|]

getPath :: IO T.Text
getPath = maybe (error "no HOME defined")
                 (T.pack.BS.unpack.(flip BS.append "/gitcontrol.db"))
                `fmap` getEnv "HOME"

instance GitControl (T.Text) where
    isAuthorized _    _     _     None  = return False
    isAuthorized path (Username rName) (RepositoryPath rName) aMode = do
        runSqlite path $ do
            uEntity <- getBy $ UniqueUser uName
            rEntity <- getBy $ UniqueRepo rName
            case (uEntity, rEntity) of
                 (Just (Entity uK _), Just (Entity rK _)) -> do
                      mEntity <- getBy $ UniqueMode rK uK
                      case mEntity of
                          Nothing -> return False
                          Just (Entity _ (Mode _ _ access)) -> return $ aMode <= (read access :: AccessMode)
                 _ -> return False

doCreateDatabase = getPath >>= \h ->
    runSqlite h $ runMigration migrateAll

main :: IO ()
main =
   (maybe (error "no HOME defined") (flip BS.append "/")) `fmap` getEnv "HOME"
       >>= \h -> defaultMain h (return $ T.pack $ BS.unpack $ BS.append h "gitcontrol.db")
