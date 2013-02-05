{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
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
        hFlush stdout

writebs :: Handle -> B.ByteString -> IO ()
writebs h cmd = do
        B.hPut h cmd
        hPutStr h "\r\n"
        printf ">>> %s\n" . T.unpack $ TE.decodeUtf8 cmd
        hFlush stdout

waitFor :: Handle -> String -> IO ()
waitFor h str = do
        ln <- hGetLine h
        putStrLn $ "<<< " <> ln
        unless (str `isPrefixOf` ln) (waitFor h str)
        hFlush stdout

tlsWrite :: Context -> String -> IO ()
tlsWrite ctx cmd = do
        sendData ctx . BL.fromChunks
                     . (:[])
                     . TE.encodeUtf8
                     . T.pack
                     . printf "%s\r\n"
                     $ cmd
        printf ">>> %s\n" cmd
        hFlush stdout

tlsWritebs :: Context -> B.ByteString -> IO ()
tlsWritebs ctx cmd = do
        sendData ctx $ BL.fromChunks [cmd, "\r\n"]
        printf ">>> %s\n" . T.unpack $ TE.decodeUtf8 cmd
        hFlush stdout

tlsBsPutCrLf :: Context -> B.ByteString -> IO ()
tlsBsPutCrLf ctx s = do
        sendData ctx $ BL.fromChunks [s]
        sendData ctx crlf
        contextFlush ctx

crlf :: BL.ByteString
crlf = BCL.pack "\r\n"

-- TODO: Add timeout
tlsWaitFor :: Context -> String -> IO ()
tlsWaitFor ctx str = do
        -- recvData ctx >>= printf . T.unpack . ("<<< " <>) . TE.decodeUtf8
        -- [ln] <- take 1
        --       . filter (T.isPrefixOf (T.pack str))
        lns <- T.lines . TE.decodeUtf8 <$> recvData ctx
        -- printf . T.unpack $ "<<< " <> ln
        forM_ lns $ printf . T.unpack . ("<<< " <>) . (<> "\n")
        hFlush stdout
        case filter (T.isPrefixOf (T.pack str)) lns of
            [] -> tlsWaitFor ctx str
            _  -> return ()

sendMailTls :: HostName -> UserName -> Password -> Mail -> IO ()
sendMailTls host = sendMailTls' host 587

b64 :: String -> B.ByteString
b64 = B64.encode . B.pack . map (toEnum . fromEnum)

-- TODO: option to pass in TLSParams
-- TODO: run in EitherT.

writeAddress :: Context -> T.Text -> Address -> IO ()
writeAddress ctx cmd Address{..} = do
        tlsWrite ctx . T.unpack $ cmd <> ":<" <> addressEmail <> ">"
        tlsWaitFor ctx "250"

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
        let sendLine = tlsBsPutCrLf ctx
        putStrLn "handshake"
        handshake ctx

        putStrLn "login"
        tlsWrite ctx "EHLO"
        tlsWaitFor ctx "250"
        tlsWrite ctx "AUTH LOGIN"
        tlsWaitFor ctx "334"
        tlsWritebs ctx $ b64 user
        tlsWaitFor ctx "334"
        tlsWritebs ctx $ b64 passwd
        tlsWaitFor ctx "235"

        putStrLn "renderAndSend"
        mailbs <- renderMail' mail
        writeAddress ctx "MAIL FROM" $ mailFrom mail
        let rcpts = concatMap (\f -> f mail) [mailTo, mailCc, mailBcc]
        forM_ rcpts $ writeAddress ctx "RCPT TO"
        tlsWrite ctx "DATA"
        tlsWaitFor ctx "354"

        putStrLn $ "SENDING " <> show mailbs
        mapM_ sendLine . concatMap BCL.toChunks $ split mailbs
        mapM_ sendLine $ BCL.toChunks dot

        putStrLn "bye"
        bye ctx

    where 
        split = map (padDot . stripCR) . BCL.split '\r'
        -- remove \r at the end of a line
        stripCR s = if cr `BL.isSuffixOf` s then BL.init s else s
        -- duplicate . at the start of a line
        padDot s = if dot `BL.isPrefixOf` s then dot <> s else s
        cr = BCL.pack "\r"
        dot = BCL.pack "."

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

