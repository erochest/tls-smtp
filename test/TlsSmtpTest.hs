module Main where


import Control.Applicative
import Data.Maybe (maybe)
import Network.Socket (PortNumber(..))
import System.Environment (getEnv, getEnvironment)


main :: IO ()
main = do
        env  <- getEnvironment
        host <- getEnv "SMTP_HOST"
        let port = PortNum . maybe 587 read $ lookup "SMTP_PORT" env
        putStrLn host
        print port
