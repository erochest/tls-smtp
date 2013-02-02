{-# LANGUAGE OverloadedStrings #-}

-- | This is a fairly simple module that just connects the smtp-mail package
-- with the tls package. Hopefully this will make it easier to connect to, say,
-- GMail.
--
-- This is just this SO answer, http://stackoverflow.com/a/13634590/34864,
-- wrapped in a module and packaged up.

module Network.Mail.SMTP.TLS
    ( sendMailTls
    , sendMailTls'
    , ciphers
    , tlsParams
    ) where

import           Control.Applicative
import           Control.Monad (unless, forM_)
import qualified Crypto.Random.AESCtr as RNG
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.Lazy (fromChunks)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.List (isPrefixOf)
import           Data.Monoid
import           Network
import           Network.Mail.Mime
import           Network.Mail.SMTP
import           Network.Socket
import           Network.TLS
import           Network.TLS.Extra
import           System.IO
import           Text.Printf

ciphers :: [Cipher]
ciphers = [ cipher_AES128_SHA1
          , cipher_AES256_SHA1
          , cipher_RC4_128_MD5
          , cipher_RC4_128_SHA1
          ]

tlsParams :: TLSParams
tlsParams = defaultParamsClient { pCiphers = ciphers }

write :: Handle -> String -> IO ()
write h cmd = do
        hPrintf h "%s\r\n" cmd
        printf ">>> %s\n" cmd

writebs :: Handle -> B.ByteString -> IO ()
writebs h cmd = do
        B.hPut h cmd
        hPutStr h "\r\n"
        printf ">>> %s\n" . T.unpack $ TE.decodeUtf8 cmd

waitFor :: Handle -> String -> IO ()
waitFor h str = do
        ln <- hGetLine h
        putStrLn $ "<<< " <> ln
        unless (str `isPrefixOf` ln) (waitFor h str)

tlsWrite :: Context -> String -> IO ()
tlsWrite ctx cmd = do
        sendData ctx . fromChunks
                     . (:[])
                     . TE.encodeUtf8
                     . T.pack
                     . printf "%s\r\n"
                     $ cmd
        printf ">>> %s\n" cmd

tlsWritebs :: Context -> B.ByteString -> IO ()
tlsWritebs ctx cmd = do
        sendData ctx $ fromChunks [cmd, "\r\n"]
        printf ">>> %s\n" . T.unpack $ TE.decodeUtf8 cmd

tlsWaitFor :: Context -> String -> IO ()
tlsWaitFor ctx str = do
        -- [ln] <- take 1
        --       . filter (T.isPrefixOf (T.pack str))
        lns <- takeWhile (not . T.isPrefixOf (T.pack str))
              . T.lines
              . TE.decodeUtf8
            <$> recvData ctx
        -- printf . T.unpack $ "<<< " <> ln
        forM_ lns $ printf . T.unpack . ("<<< " <>)

sendMailTls :: HostName -> UserName -> Password -> Mail -> IO ()
sendMailTls host = sendMailTls' host 587

b64 :: String -> B.ByteString
b64 = B64.encode . B.pack . map (toEnum . fromEnum)

-- TODO: option to pass in TLSParams
-- TODO: run in EitherT.

sendMailTls' :: HostName -> Int -> UserName -> Password -> Mail -> IO ()
sendMailTls' host port user passwd mail = do
        g <- RNG.makeSystem
        let pn = PortNumber $ fromIntegral port
        h <- connectTo host pn
        hSetBuffering h LineBuffering

        write h "EHLO"
        waitFor h "250-STARTTLS"
        write h "STARTTLS"
        waitFor h "220"

        ctx <- contextNewOnHandle h tlsParams g
        putStrLn "handshake"
        handshake ctx

        putStrLn "login"
        tlsWrite ctx "EHLO"
        tlsWaitFor ctx "250"
        tlsWrite ctx "AUTH LOGIN"
        tlsWritebs ctx $ b64 user
        tlsWritebs ctx $ b64 passwd
        tlsWaitFor ctx "220"

        -- _ <- login smtp user passwd
        -- putStrLn "renderAndSend"
        -- renderAndSend smtp mail
        -- putStrLn "closeSMTP"
        -- closeSMTP smtp

        putStrLn "bye"
        bye ctx

{-
        g    <- RNG.makeSystem
        putStrLn "connectSMTP'"
        smtp <- connectSMTP' host port
        let h = smtpHandle smtp

        putStrLn "waitFor 250"
        waitFor h "250-STARTTLS"
        hPrint h "STARTTLS\r\n"
        putStrLn "waitFor 220"
        waitFor h "220"
        putStrLn "contextNewOnHandle"
        con <- contextNewOnHandle h tlsParams g
        putStrLn "handshake"
        handshake con
        putStrLn "login"
        _ <- login smtp user passwd
        putStrLn "renderAndSend"
        renderAndSend smtp mail
        putStrLn "closeSMTP"
        closeSMTP smtp
        putStrLn "bye"
        bye con
        -}

