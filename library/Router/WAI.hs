module Router.WAI where

import BasePrelude
import Data.Text (Text)
import qualified Router.RequestParser as A
import qualified Router.ResponseBuilder as B
import qualified Router.Executor as C
import qualified Network.Wai as D
import qualified Network.Wai.Handler.Warp as E


routeServer :: Monad m => Int -> (forall a. m a -> IO (Either Text a)) -> A.RequestParser m B.ResponseBuilder -> IO ()
routeServer port runBase route =
  E.run port (routeApplication runBase route)

routeApplication :: Monad m => (forall a. m a -> IO (Either Text a)) -> A.RequestParser m B.ResponseBuilder -> D.Application
routeApplication runBase route =
  \request responseHandler ->
    runBase (C.route (routerRequest request) route) >>=
    responseHandler . waiResponse . either internalError id . join
  where
    internalError message =
      B.run B.internalErrorStatus
    routerRequest waiRequest =
      undefined
    waiResponse response =
      undefined
