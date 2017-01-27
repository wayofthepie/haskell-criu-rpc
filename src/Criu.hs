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
  module Proto.Criu.Rpc
  , module Lens.Family2
  -- * Request
  -- $requests
  , callCriu
  , callCriu'
  -- ** Request Builders
  -- $requestbuilders
  ,
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
--
-- @
--  > callCriu "\/var\/tmp\/criu_service.socket" (build  (type' .~ CHECK)  :: Criu_req)
--  Right (
--    Criu_resp
--      { _Criu_resp'type' = CHECK
--      , _Criu_resp'success = True
--      , _Criu_resp'dump = Nothing
--      , _Criu_resp'restore = Nothing
--      , _Criu_resp'notify = Nothing
--      , _Criu_resp'ps = Nothing
--      , _Criu_resp'crErrno = Nothing
--      , _Criu_resp'features = Nothing
--      , _Criu_resp'crErrmsg = Nothing
--      }
--  )
-- @
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

-- | Get a directory file descriptor as an int.
-- Caution - does not even attempt to close the file descriptor!
getDirFdAsInt :: FilePath -> IO Int32
getDirFdAsInt dir =
  openFd dir ReadOnly Nothing (OpenFileFlags False False False False False) >>=
    pure . fromIntegral

--------------------------------------------------------------------------------
-- Documentation

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

To send that request:

@
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

-- $requestbuilders
-- To build 'Criu_req''s you will need '.~' from "Lens.Family2", 'build' from
-- "Data.ProtoLens" and of course the lenses and types from "Proto.Criu.RPC" - both
-- "Lens.Family2" and "Proto.Criu.RPC" are re-exported from this module for convenience.
--
-- Above we saw a simple /check/ request, the next example is a /dump/ request. This one is
-- a bit more complex. Criu requires the file descriptor of an open directory when issuing a
-- /dump/ request to it. This is the directory where it will /dump/ the process tree and all
-- necessary information about it.

