
-- | This is a fairly simple module that just connects the smtp-mail package
-- with the tls package. Hopefully this will make it easier to connect to, say,
-- GMail.

module Network.Mail.SMTP.TLS
    ( sendMailTls
    , sendMailTls'
    ) where

import           Network.Mail.Mime
import           Network.Mail.SMTP
import           Network.Socket


sendMailTls :: HostName -> UserName -> Password -> Mail -> IO ()
sendMailTls = undefined

sendMailTls' :: HostName -> PortNumber -> UserName -> Password -> Mail -> IO ()
sendMailTls' = undefined

