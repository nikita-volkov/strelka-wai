module Strelka.WAI
(
  strelkaServer,
  strelkaApplication,
)
where

import BasePrelude
import Data.Text (Text)
import System.IO (stderr)
import qualified Strelka.Core.RequestParser as A
import qualified Strelka.Core.ResponseBuilder as B
import qualified Strelka.Core.Executor as C
import qualified Strelka.Core.Model as F
import qualified Network.Wai as D
import qualified Network.Wai.Handler.Warp as E
import qualified Network.HTTP.Types as G
import qualified Data.CaseInsensitive as H
import qualified Data.ByteString.Builder as I
import qualified Data.ByteString as L
import qualified Data.HashMap.Strict as J
import qualified Data.Text.IO as K


-- |
-- Given a port number, a base monad executor and a route specification, starts the Warp server.
strelkaServer :: Monad m => Int -> (forall a. m a -> IO (Either Text a)) -> A.RequestParser m B.ResponseBuilder -> IO ()
strelkaServer port runBase route =
  E.run port (strelkaApplication runBase route)

-- |
-- Given a base monad executor and a route specification, produces a WAI application.
strelkaApplication :: Monad m => (forall a. m a -> IO (Either Text a)) -> A.RequestParser m B.ResponseBuilder -> D.Application
strelkaApplication runBase route =
  \request responseHandler ->
    do
      responseEither <- fmap join (runBase (C.route (strelkaRequest request) route))
      case responseEither of
        Left msg ->
          do
            K.hPutStrLn stderr msg
            responseHandler (waiResponse internalErrorResponse)
        Right response ->
          responseHandler (waiResponse response)
  where
    internalErrorResponse =
      F.Response (F.Status 500) [] (F.OutputStream (const (const (pure ()))))

strelkaRequest :: D.Request -> F.Request
strelkaRequest waiRequest =
  F.Request method path query headers inputStream
  where
    method =
      F.Method (H.foldCase (D.requestMethod waiRequest))
    path =
      fmap F.PathSegment (D.pathInfo waiRequest)
    query =
      F.Query . maybe "" snd . L.uncons . D.rawQueryString $ waiRequest
    headers =
      J.fromList (map row (D.requestHeaders waiRequest))
      where
        row (name, value) =
          (F.HeaderName (H.foldedCase name), F.HeaderValue value)
    inputStream =
      F.InputStream (D.requestBody waiRequest)

waiResponse :: F.Response -> D.Response
waiResponse (F.Response status headerList outputStream) =
  D.responseStream (waiStatus status) (map waiHeader headerList) (waiStreamingBody outputStream)

waiStatus :: F.Status -> G.Status
waiStatus (F.Status statusCode) =
  toEnum statusCode

waiHeader :: F.Header -> G.Header
waiHeader (F.Header (F.HeaderName name) (F.HeaderValue value)) =
  (H.mk name, value)

waiStreamingBody :: F.OutputStream -> D.StreamingBody
waiStreamingBody (F.OutputStream outputStreamFn) =
  \consumeBuilder flush -> outputStreamFn (consumeBuilder . I.byteString) flush
