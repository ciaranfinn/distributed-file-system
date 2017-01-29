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
        putStrLn "Please provide a startup port >>"
        port <- getLine
        let sPort = read port :: Int
        print "Subscribing to Registry.."
        registerFs sPort
        startApp sPort


-- We Subscribe to the registry prior to running
-- the file Service
registerFs :: Int -> IO ()
registerFs port = do
        let subscriber = Subscriber getHost port "active" "fs"
        response <- (SC.runClientM (subscribe subscriber) =<< env)
        case response of
                Right a -> print "File service successfully subscribed"
                Left e -> do
                        threadDelay (3 * 3600 * 600) -- wait for a period and then retry to subscribe
                        print "Retry to subscribe"
                        registerFs port
        where env = do
               manager <- newManager defaultManagerSettings
               let (host,port) = registry_service
               return (SC.ClientEnv manager (SC.BaseUrl SC.Http host port ""))
