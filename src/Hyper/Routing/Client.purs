module Hyper.Routing.Client
       ( class HasClients
       , getClients
       , asClients
       ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except.Trans (throwError)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Hyper.Routing (type (:>), type (:<|>), (:<|>), Capture, CaptureAll, Handler, Lit)
import Hyper.Routing.PathPiece (class ToPathPiece, toPathPiece)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Type.Proxy (Proxy(..))

type RequestBuilder = { path :: Array String }

emptyRequestBuilder :: RequestBuilder
emptyRequestBuilder = { path: [] }

appendSegment :: String -> RequestBuilder -> RequestBuilder
appendSegment segment req =
  req { path = req.path <> singleton segment }

toAffjaxRequest :: RequestBuilder -> AffjaxRequest Unit
toAffjaxRequest req = defaultRequest { url = "/" <> joinWith "/" req.path }

class HasClients r mk | r -> mk where
  getClients :: Proxy r -> RequestBuilder -> mk

instance hasClientsLit :: (HasClients sub subMk, IsSymbol lit)
                          => HasClients (Lit lit :> sub) subMk where
  getClients _ req =
    getClients (Proxy :: Proxy sub) (appendSegment segment req)
    where
      segment = reflectSymbol (SProxy :: SProxy lit)

instance hasClientsCapture :: (HasClients sub subMk, IsSymbol c, ToPathPiece t)
                              => HasClients (Capture c t :> sub) (t -> subMk) where
  getClients _ req x =
    getClients (Proxy :: Proxy sub) (appendSegment (toPathPiece x) req)

instance hasClientsCaptureAll :: (HasClients sub subMk, IsSymbol c, ToPathPiece t)
                                 => HasClients (CaptureAll c t :> sub) (Array t -> subMk) where
  getClients _ req xs =
    getClients (Proxy :: Proxy sub) (foldl (flip appendSegment) req (map toPathPiece xs))

instance hasClientsHandlerJson :: (DecodeJson b)
                                  => HasClients (Handler "GET" Json b) (Aff (ajax :: AJAX | e) b) where
  getClients _ req = do
    r <- affjax (toAffjaxRequest req)
    case decodeJson r.response of
      Left err -> throwError (error err)
      Right x -> pure x

instance hasClientsHandlerString :: HasClients (Handler "GET" String String) (Aff (ajax :: AJAX | e) String) where
  getClients _ req =
    _.response <$>  affjax (toAffjaxRequest req)

instance hasClientsAlt :: (HasClients c1 mk1, HasClients c2 mk2)
                          => HasClients (c1 :<|> c2) (mk1 :<|> mk2) where
  getClients _ req =
    getClients (Proxy :: Proxy c1) req :<|> getClients (Proxy :: Proxy c2) req

asClients :: forall r mk. HasClients r mk => Proxy r -> mk
asClients p = getClients p emptyRequestBuilder
