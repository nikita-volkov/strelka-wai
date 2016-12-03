module Strelka.WAI
(
  strelkaServer,
  strelkaApplication,
)
where

import BasePrelude
import Data.Text (Text)
import System.IO (stderr)
import qualified Strelka.RequestParser as A
import qualified Strelka.ResponseBuilder as B
import qualified Strelka.Executor as C
import qualified Strelka.Model as F
import qualified Network.Wai as D
import qualified Network.Wai.Handler.Warp as E
import qualified Network.HTTP.Types as G
import qualified Data.CaseInsensitive as H
import qualified Data.ByteString.Builder as I
import qualified Data.HashMap.Strict as J
import qualified Data.Text.IO as K


strelkaServer :: Monad m => Int -> (forall a. m a -> IO (Either Text a)) -> A.RequestParser m B.ResponseBuilder -> IO ()
strelkaServer port runBase route =
  E.run port (strelkaApplication runBase route)

strelkaApplication :: Monad m => (forall a. m a -> IO (Either Text a)) -> A.RequestParser m B.ResponseBuilder -> D.Application
strelkaApplication runBase route =
  \request responseHandler ->
    do
      responseEither <- fmap join (runBase (C.route (strelkaRequest request) route))
      case responseEither of
        Left msg ->
          do
            K.hPutStrLn stderr msg
            responseHandler (waiResponse (B.run B.internalErrorStatus))
        Right response ->
          responseHandler (waiResponse response)

strelkaRequest :: D.Request -> F.Request
strelkaRequest waiRequest =
  F.Request method path query headers inputStream
  where
    method =
      F.Method (H.foldCase (D.requestMethod waiRequest))
    path =
      F.Path (D.rawPathInfo waiRequest)
    query =
      J.fromList (map row (D.queryString waiRequest))
      where
        row (name, value) =
          (F.ParamName name, F.ParamValue value)
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
