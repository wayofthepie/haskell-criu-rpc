{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Criu
-- Description : Convenience functions for the CRIU RPC API.
-- Copyright   : (C) 2017 Stephen O'Brien
-- License     : MIT (see the file LICENSE)
-- Maintainer  : Stephen O'Brien <wayofthepie@gmail.com>
-- Stability   : experimental
--
-- This module contains some convenience functions and types for building
-- requests to, and actually calling, the Checkpoint/Restore In Userspace
-- (criu) RPC API - see <https://criu.org/RPC>.

module Criu (
  -- * Request
  -- $requests
  callCriu
  , callCriu'
  -- ** Request Builders
  -- $requestbuilders
  , checkRequest
  , dumpRequest
  , dumpRequestOpts
  , openDir
  , closeDirFd
  ) where

import Control.Exception.Base (IOException, bracket, try)
import GHC.Int
import Lens.Family2 ((.~))
import Data.ProtoLens (build, decodeMessage, encodeMessage)
import Proto.Criu.Rpc
import Network.Socket (Family(AF_UNIX), SocketType(SeqPacket), SockAddr(SockAddrUnix), close, connect, socket)
import Network.Socket.ByteString (recv, send)
import System.Posix.IO
import System.Posix.Types

{- $requests
The simplest example of a call to CRIU is a __/check/__ request. For convenience
this module re-exports some libraries used for building 'Criu_req's.

To build a __/check/__ request:

@
> let checkRequest = build  (type' .~ CHECK)  :: Criu_req
Criu_req {
  _Criu_req'type' = CHECK
  , _Criu_req'opts = Nothing
  , _Criu_req'notifySuccess = Nothing
  , _Criu_req'keepOpen = Nothing
  , _Criu_req'features = Nothing
  }
@

'checkRequest' is also a function exposed in this module. To send that request:

@
> let checkRequest = build  (type' .~ CHECK)  :: Criu_req
> callCriu "\/var\/tmp\/criu_service.socket" checkRequest
Right (
  Criu_resp
    { _Criu_resp'type' = CHECK
    , _Criu_resp'success = True
    , _Criu_resp'dump = Nothing
    , _Criu_resp'restore = Nothing
    , _Criu_resp'notify = Nothing
    , _Criu_resp'ps = Nothing
    , _Criu_resp'crErrno = Nothing
    , _Criu_resp'features = Nothing
    , _Criu_resp'crErrmsg = Nothing
    }
)
@

'callCriu' may throw an 'IOException', to wrap these, specifically,
in 'Either' use 'callCriu'' instead.
-}


-- | Send a request to a criu service, but wraps up IOExceptions in an `Either`.
callCriu' :: FilePath -> Criu_req -> IO (Either String Criu_resp)
callCriu' fp req = do
  eitherResp <- try (callCriu fp req)
  case eitherResp of
    Right resp -> pure resp
    Left (e :: IOException) -> pure . Left . show $ e


-- | Send request to a criu service. Takes a filepath to the criu service
-- socket and a criu request. Returns an `Either` of the response or a
-- `String` representing an error if there is an issue decoding the
-- response from the criu service.
callCriu :: FilePath -> Criu_req -> IO (Either String Criu_resp)
callCriu fp req = do
  resp <- withSocket $ \sock -> do
    connect sock (SockAddrUnix fp)
    send sock (encodeMessage req)
    recv sock 1024
  pure (decodeMessage resp :: Either String Criu_resp)
 where
  withSocket f = bracket
    (socket AF_UNIX SeqPacket 0)
    (close)
    (\sock -> f sock)


-- | Build some options.
criuOpts :: Int32 -> Int32 -> Criu_opts
criuOpts procId dirFd =
  build ((imagesDirFd .~ dirFd) . (pid .~ procId) . (shellJob .~ True))
{- $requestbuilders
To build 'Criu_req''s you will need '.~' from "Lens.Family2", 'build' from
"Data.ProtoLens" and of course the lenses and types from "Proto.Criu.Rpc".

Above we saw a simple /check/ request, the next example is a /dump/ request. This one is
a bit more complex. Criu requires the file descriptor of an open directory when issuing a
/dump/ request to it. This is the directory where it will /dump/ the process tree and all
necessary information about the process. Below we set the pid to __/11612/__ and we also set
__/shellJob/__ to 'True', which tells criu that the process we want to dump is running in a shell.

@
> fd <- getDirFdAsInt "\/var\/tmp\/dump"
> let dumpReqOpts = build ((imagesDirFd .~ fd) . (pid .~ 11612) . (shellJob .~ True)) :: Criu_opts
> let dumpRequest = build ((type' .~ DUMP) . (opts .~ dumpReqOpts)) :: Criu_req
> callCriu' "\/var\/tmp\/criu_service.socket" dumpRequest
Right (
  Criu_resp {
    _Criu_resp'type' = DUMP
    , _Criu_resp'success = True
    , _Criu_resp'dump = Just (
      Criu_dump_resp {
        _Criu_dump_resp'restored = Just False
      }
    ), _Criu_resp'restore = Nothing
    , _Criu_resp'notify = Nothing
    , _Criu_resp'ps = Nothing
    , _Criu_resp'crErrno = Nothing
    , _Criu_resp'features = Nothing
    , _Criu_resp'crErrmsg = Nothing
    })
@
-}


-- | Builds a check request.
checkRequest :: Criu_req
checkRequest = build (type' .~ CHECK)


-- | Builds a dump request from the give Criu_opts.
dumpRequest :: Criu_opts -> Criu_req
dumpRequest options = build ((type' .~ DUMP) . (opts .~ options))


-- | Builds a minimal dump based 'Criu_opts' from a file descriptor pointing to
-- the location of the open directory you wish to dump the process tree and
-- the pid of the process tree you wish to dump. Note that really the only
-- requirement is to set 'imagesDirFd', not setting the pid will dump the
-- /calling/ process, this is not something that seems likely when using
-- this client.
dumpRequestOpts :: Fd -> Int32 -> Criu_opts
dumpRequestOpts fd procId =
  build ((imagesDirFd .~ (fromIntegral fd)) . (pid .~ procId))

-- | Open a directory, returning its file descriptor.
-- Caution - make sure to close the file descriptor! Can use 'closeDirFd'.
openDir :: FilePath -> IO Fd
openDir dir =
  openFd dir ReadOnly Nothing (OpenFileFlags False False False False False)


-- | A convenience method, can be used to close the 'FD' returned by 'openDir'.
-- This is literally just "System.Posix.IO"'s 'closeFd'.
closeDirFd :: Fd -> IO ()
closeDirFd = closeFd


