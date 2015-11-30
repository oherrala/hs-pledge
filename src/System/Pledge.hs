{-# LANGUAGE ForeignFunctionInterface #-}

module System.Pledge (Promise(..), pledge) where

import Data.List (intersperse, nub)
import Data.Char (toLower)
import Foreign (Ptr, nullPtr)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString, withCString)
import System.FilePath (FilePath)

foreign import ccall "unistd.h pledge" c_pledge :: CString -> Ptr [CString] -> IO Int

-- | List of promises to give
type Promises = [Promise]

-- | Allowed promises. See OpenBSD's pledge(2) for documentation
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


-- | Pledge a program
pledge :: Promises -> [FilePath] -> IO ()

-- special case for completely empty pledge. Useful? Maybe not.
pledge [] _ = throwErrnoIfMinus1_ "pledge" $ c_pledge nullPtr nullPtr

-- Generic case, but we don't support giving whilelist of paths yet
pledge proms [] =
  withCString (promises proms) $ \c_proms ->
      let c_paths = nullPtr in
            throwErrnoIfMinus1_ "pledge" $ c_pledge c_proms c_paths

pledge _ _ = error "pledge does not support [FilePath] yet"


-- | Convert Promise into String
--
-- >>> promise Stdio
-- "stdio"
promise :: Promise -> String
promise = map toLower . show


-- | Convert list of Promises into single String
--
-- >>> promises [Stdio]
-- "stdio"
--
-- >>> promises [Stdio, Coredump]
-- "stdio coredump"
--
-- >>> promises [Stdio, Coredump, Coredump, Stdio]
-- "stdio coredump"
--
-- >>> promises []
-- ""
--
-- >>> promises [None]
-- ""
--
-- >>> promises [Stdio, Coredump, None]
-- ""
promises :: Promises -> String
promises [] = ""
promises proms = if elem None proms
                 then ""
                 else concat $ intersperse " " $ map promise $ nub proms
