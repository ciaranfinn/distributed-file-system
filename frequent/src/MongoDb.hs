{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module MongoDb (drainCursor, runMongo, logLevel) where

  import           Database.MongoDB
  import           Data.Text                    (pack, unpack)
  import           Control.Monad                (when)
  import           Control.Monad.IO.Class
  import           Control.Monad.Trans.Except   (ExceptT)
  import           Control.Monad.Trans.Resource
  import           System.Environment           (lookupEnv)

  --- Run a database command within mongo ---
  runMongo :: Action IO a -> IO a
  runMongo act  = do
    ip <- mongoDbIp
    port <- mongoDbPort
    database <- mongoDbDatabase
    pipe <- connect (host ip)
    ret <- runResourceT $ liftIO $ access pipe master (pack database) act
    close pipe
    return ret



  drainCursor :: Cursor -> Action IO [Document]
  drainCursor cur = drainCursor' cur []
    where
      drainCursor' cur res  = do
        batch <- nextBatch cur
        if null batch
          then return res
          else drainCursor' cur (res ++ batch)



  --- Environment Var Setup ---
  mongoDbIp :: IO String
  mongoDbIp = defEnv "MONGODB_IP" id "localhost" True

  mongoDbPort :: IO Integer
  mongoDbPort = defEnv "MONGODB_PORT" read 27017 False

  mongoDbDatabase :: IO String
  mongoDbDatabase = defEnv "MONGODB_DATABASE" id "FILESTORE" True

  logLevel :: IO String
  logLevel = defEnv "LOG_LEVEL" id "DEBUG" True



  defEnv :: Show a
                => String        -- Environment Variable name
                -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
                -> a             -- default value to use if environment variable is not set
                -> Bool          -- True if we should warn if environment variable is not set
                -> IO a
  defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
        Just s  -> return $ fn s
        Nothing -> return def
