{-# LANGUAGE ForeignFunctionInterface #-}

module System.Pledge (Promise(..), pledge) where

import Data.List (intersperse)
import Data.Char (toLower)

import Foreign
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString, withCString)

foreign import ccall "unistd.h pledge" c_pledge :: CString -> CString -> IO Int

type Promises = [Promise]

data Promise = Rpath
             | Wpath
             | Cpath
             | Stdio
             | Tmppath
             | Dns
             | Inet
             | Flock
             | Unix
             | Id
             | Ioctl
             | Getpw
             | Proc
             | Settime
             | Fattr
             | Protexec
             | Tty
             | Sendfd
             | Recvfd
             | Exec
             | Route
             | Mcast
             | Vminfo
             | Ps
             | Coredump
             | Disklabel
             | Pf
             | None
             deriving (Eq, Show)

promise :: Promise -> String
promise = map toLower . show

promises :: Promises -> String
promises = concat . intersperse " " . map promise

pledge :: Promises -> String -> IO ()
pledge proms paths =
  withCString (promises proms) $ \c_proms ->
      withCString paths $ \c_paths ->
            throwErrnoIfMinus1_ "pledge" $ c_pledge c_proms c_paths
