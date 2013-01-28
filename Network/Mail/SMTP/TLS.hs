
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

import           Control.Monad (unless)
import qualified Crypto.Random.AESCtr as RNG
import           Data.List (isPrefixOf)
import           Network
import           Network.Mail.Mime
import           Network.Mail.SMTP
import           Network.Socket
import           Network.TLS
import           Network.TLS.Extra
import           System.IO

ciphers :: [Cipher]
ciphers = [ cipher_AES128_SHA1
          , cipher_AES256_SHA1
          , cipher_RC4_128_MD5
          , cipher_RC4_128_SHA1
          ]

tlsParams :: TLSParams
tlsParams = defaultParamsClient { pCiphers = ciphers }

waitFor :: Handle -> String -> IO ()
waitFor h str = do
        ln <- hGetLine h
        unless (str `isPrefixOf` ln) (waitFor h str)

sendMailTls :: HostName -> UserName -> Password -> Mail -> IO ()
sendMailTls host = sendMailTls' host (PortNum 587)

-- TODO: run in EitherT.
sendMailTls' :: HostName -> PortNumber -> UserName -> Password -> Mail -> IO ()
sendMailTls' host port user passwd mail = do
        g    <- RNG.makeSystem
        smtp <- connectSMTP' host port
        let h = smtpHandle smtp
        waitFor h "250-STARTTLS"
        hPrint h "STARTTLS\r\n"
        waitFor h "220"
        con <- contextNewOnHandle h tlsParams g
        handshake con
        _ <- login smtp user passwd
        renderAndSend smtp mail
        closeSMTP smtp
        bye con

