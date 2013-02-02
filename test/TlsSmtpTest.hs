{-# LANGUAGE OverloadedStrings #-}

module Main where

{-
import Network.Socket
import Network
import Network.TLS
import Network.TLS.Extra

import Crypto.Random
import qualified Crypto.Random.AESCtr as RNG
import System.IO
import Text.Printf

import Data.Monoid
import Control.Monad (when)
import Data.List (isPrefixOf)

ciphers :: [Cipher]
ciphers =
        [ cipher_AES128_SHA1
        , cipher_AES256_SHA1
        , cipher_RC4_128_MD5
        , cipher_RC4_128_SHA1
        ]

main :: IO ()
main = emailGmail2

cWrite :: Handle -> String -> IO ()
cWrite h s  = do
    hPrintf h "%s\r\n" s
    printf    "> %s\n" s

cWaitFor :: Handle -> String -> IO ()
cWaitFor h str = do
    ln <- hGetLine h
    putStrLn ln
    when (not $ str `isPrefixOf` ln) (cWaitFor h str)

emailGmail2 = do
    let
        host = "smtp.gmail.com"
        port = 587
        params = defaultParamsClient{pCiphers = ciphers}
        p = PortNumber (fromIntegral port)
    putStrLn $ "port = " <> show p
    g <- RNG.makeSystem
    h <- connectTo host p
    hSetBuffering h LineBuffering
    cWrite h "EHLO"
    cWaitFor h "250-STARTTLS"
    cWrite h "STARTTLS"
    cWaitFor h "220"
    con <- contextNewOnHandle h params g
    handshake con
    bye con
-}

import Control.Applicative
import Data.Maybe (maybe)
import Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time
import Data.Word
import Network.Mail.Mime
import Network.Mail.SMTP.TLS
import Network (PortID(..))
import Network.Socket (PortNumber(..))
import System.Environment (getEnv, getEnvironment)


from    = Address Nothing "erochest@gmail.com"
to      = Address Nothing "eric@ericrochester.com"
subject = "Test Email"
body    = "email body" :: TL.Text
html    = "<h1>HTML</h1>\n<p>Hi-ya!</p>" :: TL.Text

-- From smtp-mail:

-- | Construct a plain text 'Part'
plainTextPart :: TL.Text -> Part
plainTextPart = Part "text/plain; charset=utf-8" 
                     QuotedPrintableText Nothing [] . TLE.encodeUtf8

-- | Construct an html 'Part'
htmlPart :: TL.Text -> Part
htmlPart = Part "text/html; charset=utf-8" 
                QuotedPrintableText Nothing [] . TLE.encodeUtf8

main :: IO ()
main = do
        env  <- getEnvironment
        host <- getEnv "SMTP_HOST"
        -- let port = PortNumber . maybe 587 read $ lookup "SMTP_PORT" env
        let port = 587

        user <- getEnv "SMTP_USER"
        pass <- getEnv "SMTP_PASS"
        now  <- getCurrentTime
        mail <- simpleMail from to subject
                           (body <> TL.pack (show now))
                           (html <> "<p>" <> TL.pack (show now) <> "</p>")
                           []
        sendMailTls' host port user pass mail
        putStrLn "sent."

