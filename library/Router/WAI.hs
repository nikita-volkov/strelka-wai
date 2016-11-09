module Router.WAI
(
  routerServer,
  routerApplication,
)
where

import BasePrelude
import Data.Text (Text)
import qualified Router.RequestParser as A
import qualified Router.ResponseBuilder as B
import qualified Router.Executor as C
import qualified Router.Model as F
import qualified Network.Wai as D
import qualified Network.Wai.Handler.Warp as E
import qualified Network.HTTP.Types as G
import qualified Data.CaseInsensitive as H
import qualified Data.ByteString.Builder as I
import qualified Data.HashMap.Strict as J


routerServer :: Monad m => Int -> (forall a. m a -> IO (Either Text a)) -> A.RequestParser m B.ResponseBuilder -> IO ()
routerServer port runBase route =
  E.run port (routerApplication runBase route)

routerApplication :: Monad m => (forall a. m a -> IO (Either Text a)) -> A.RequestParser m B.ResponseBuilder -> D.Application
routerApplication runBase route =
  \request responseHandler ->
    runBase (C.route (routerRequest request) route) >>=
    responseHandler . waiResponse . either internalError id . join
  where
    internalError message =
      B.run B.internalErrorStatus

routerRequest :: D.Request -> F.Request
routerRequest waiRequest =
  F.Request method path query headers inputStream
  where
    method =
      F.Method (D.requestMethod waiRequest)
    path =
      F.Path (D.rawPathInfo waiRequest)
    query =
      F.Query (D.rawQueryString waiRequest)
    headers =
      J.fromList (map row (D.requestHeaders waiRequest))
      where
        row (name, value) =
          (F.HeaderName (H.foldedCase name), F.HeaderValue (H.foldCase value))
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
