{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

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
import           Data.Monoid
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

class Writeable a where
        toCommand :: a -> BL.ByteString
        toDebug   :: a -> String

instance Writeable String where
        toCommand = BL.fromChunks
                  . (:[])
                  . TE.encodeUtf8
                  . T.pack
                  . printf "%s\r\n"
        toDebug = id

instance Writeable B.ByteString where
        toCommand = BL.fromChunks . (:["\r\n"])
        toDebug   = T.unpack . TE.decodeUtf8

instance Writeable BCL.ByteString where
        toCommand = (<> crlf)
        toDebug   = toDebug . mconcat . BCL.toChunks

tlsWrite :: Writeable a => Context -> a -> IO ()
tlsWrite ctx cmd = do
        sendData ctx $ toCommand cmd
        contextFlush ctx
        printf ">>> %s\n" $ toDebug cmd
        hFlush stdout

crlf :: BL.ByteString
crlf = BCL.pack "\r\n"

-- TODO: Add timeout
tlsWaitFor :: Context -> T.Text -> IO ()
tlsWaitFor ctx str = do
        lns <- T.lines . TE.decodeUtf8 <$> recvData ctx
        forM_ lns $ printf . T.unpack . ("<<< " <>) . (<> "\n")
        hFlush stdout
        case filter (T.isPrefixOf str) lns of
            [] -> tlsWaitFor ctx str
            _  -> return ()

tlsWriteWait :: Writeable a => Context -> a -> T.Text -> IO ()
tlsWriteWait ctx cmd waitFor = tlsWrite ctx cmd >> tlsWaitFor ctx waitFor

sendMailTls :: HostName -> UserName -> Password -> Mail -> IO ()
sendMailTls host = sendMailTls' host 587

b64 :: String -> B.ByteString
b64 = B64.encode . B.pack . map (toEnum . fromEnum)

-- TODO: option to pass in TLSParams
-- TODO: run in EitherT.

writeAddress :: Context -> T.Text -> Address -> IO ()
writeAddress ctx cmd Address{..} =
        tlsWriteWait ctx (T.unpack $ cmd <> ":<" <> addressEmail <> ">") "250"

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
        let sendLine = tlsWrite ctx
        putStrLn "handshake"
        handshake ctx

        putStrLn "login"
        tlsWriteWait ctx ("EHLO" :: String) "250"
        tlsWriteWait ctx ("AUTH LOGIN" :: String) "334"
        tlsWriteWait ctx (b64 user) "334"
        tlsWriteWait ctx (b64 passwd) "235"

        putStrLn "renderAndSend"
        mailbs <- renderMail' mail
        writeAddress ctx "MAIL FROM" $ mailFrom mail
        let rcpts = concatMap (\f -> f mail) [mailTo, mailCc, mailBcc]
        forM_ rcpts $ writeAddress ctx "RCPT TO"
        tlsWriteWait ctx ("DATA" :: String) "354"

        mapM_ sendLine . concatMap BCL.toChunks $ split mailbs
        mapM_ sendLine $ BCL.toChunks dot
        tlsWaitFor ctx "250"

        putStrLn "bye"
        bye ctx

    where 
        split = map (padDot . stripCR) . BCL.split '\n'
        -- remove \r at the end of a line
        stripCR s = if cr `BL.isSuffixOf` s then BL.init s else s
        -- duplicate . at the start of a line
        padDot s = if dot `BL.isPrefixOf` s then dot <> s else s
        cr = BCL.pack "\r"
        dot = BCL.pack "."

