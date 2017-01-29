module Main where

import Lib
import Frequent
import RegistryAPI
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (defaultManagerSettings,newManager)
import           Control.Concurrent           (forkIO, threadDelay)


main :: IO ()
main = do
        registerFs
        startApp


-- We Subscribe to the registry prior to running
-- the file Service
registerFs :: IO ()
registerFs = do
        let subscriber = Subscriber "999.999.999.999" 80 "alive" "fs"
        response <- (SC.runClientM (subscribe subscriber) =<< env)
        case response of
                Right a -> print "File service successfully subscribed"
                Left e -> do
                        threadDelay (3 * 3600 * 600)
                        print "Retry to subscribe"
                        registerFs

        where env = do
               manager <- newManager defaultManagerSettings
               let (host,port) = registry_service
               return (SC.ClientEnv manager (SC.BaseUrl SC.Http host port ""))
