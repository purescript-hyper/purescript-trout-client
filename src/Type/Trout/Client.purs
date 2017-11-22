module Type.Trout.Client
       ( class HasClients
       , getClients
       , class HasMethodClients
       , getMethodClients
       , asClients
       , BaseURI(..)
       ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except.Trans (throwError)
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method as Method
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.URI.Authority (Authority, printAuthority)
import Data.URI.Scheme (URIScheme, printScheme)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:<|>), type (:=), type (:>), Capture, CaptureAll, Lit, Method, QueryParam, QueryParams, Resource)
import Type.Trout.ContentType.HTML (HTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.PathPiece (class ToPathPiece, toPathPiece)
import Type.Trout.Record as Record

newtype BaseURI = BaseURI { scheme :: URIScheme , authority :: Authority }

derive instance eqBaseURI :: Eq BaseURI
derive instance genericBaseURI :: Generic BaseURI _
derive instance newtypeBaseURI :: Newtype BaseURI _
derive instance ordBaseURI :: Ord BaseURI
instance showScheme :: Show BaseURI where
  show = genericShow

instance hasClientsBaseURI :: ( HasClients sub subMk
                              )
                              => HasClients (BaseURI :> sub) (BaseURI -> subMk) where
  getClients _ req (BaseURI {authority, scheme}) =
    getClients (Proxy :: Proxy sub) (prependSegment schemeAuthority req)
      where
        schemeAuthority = printScheme scheme <> printAuthority authority

type RequestBuilder = { path :: Array String }

emptyRequestBuilder :: RequestBuilder
emptyRequestBuilder = { path: [] }

prependSegment :: String -> RequestBuilder -> RequestBuilder
prependSegment segment req =
  req { path = singleton segment <> req.path }

appendSegment :: String -> RequestBuilder -> RequestBuilder
appendSegment segment req =
  req { path = req.path <> singleton segment }

toAffjaxRequest :: RequestBuilder -> AffjaxRequest Unit
toAffjaxRequest req = defaultRequest { url = joinWith "/" req.path }

class HasClients r mk | r -> mk where
  getClients :: Proxy r -> RequestBuilder -> mk

instance hasClientsAlt :: ( HasClients c1 mk1
                          , HasClients c2 (Record mk2)
                          , IsSymbol name
                          , RowCons name mk1 mk2 out
                          )
                          => HasClients (name := c1 :<|> c2) (Record out) where
  getClients _ req = Record.insert name first rest
    where
      name = SProxy :: SProxy name
      first = getClients (Proxy :: Proxy c1) req
      rest = getClients (Proxy :: Proxy c2) req

instance hasClientsNamed :: ( HasClients c mk
                          , IsSymbol name
                          , RowCons name mk () out
                          )
                          => HasClients (name := c) (Record out) where
  getClients _ req = Record.insert name clients {}
    where
      name = SProxy :: SProxy name
      clients = getClients (Proxy :: Proxy c) req

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

instance hasClientsQueryParam :: (HasClients sub subMk, IsSymbol c, ToPathPiece t)
                                 => HasClients (QueryParam c t :> sub) (Maybe t -> subMk) where
  getClients _ req x =
    getClients (Proxy :: Proxy sub) (foldl (flip appendSegment) req (map toPathPiece x))

instance hasClientsQueryParams :: (HasClients sub subMk, IsSymbol c, ToPathPiece t)
                                  => HasClients (QueryParams c t :> sub) (Array t -> subMk) where
  getClients _ req x =
    getClients (Proxy :: Proxy sub) (foldl (flip appendSegment) req (map toPathPiece x))

instance hasClientsResource :: (HasClients methods clients)
                              => HasClients (Resource methods) clients where
  getClients _ req =
    getClients (Proxy :: Proxy methods) req

instance hasClientsMethodAlt :: ( IsSymbol method
                                , HasMethodClients method repr cts mk1
                                , HasClients methods (Record mk2)
                                , RowCons method mk1 mk2 out
                                )
                                => HasClients
                                   (Method method repr cts :<|> methods)
                                   (Record out) where
  getClients _ req =
    Record.insert method first rest
    where
      method = SProxy :: SProxy method
      cts = Proxy :: Proxy cts
      first = getMethodClients method cts req
      rest = getClients (Proxy :: Proxy methods) req

instance hasClientsMethod :: ( IsSymbol method
                             , HasMethodClients method repr cts mk1
                             , RowCons method mk1 () out
                             )
                             => HasClients (Method method repr cts) (Record out) where
  getClients _ req =
    Record.insert method clients {}
    where
      method = SProxy :: SProxy method
      cts = Proxy :: Proxy cts
      clients = getMethodClients method cts req

toMethod :: forall m
          . IsSymbol m
          => SProxy m
         -> Either Method.Method Method.CustomMethod
toMethod p = Method.fromString (reflectSymbol p)


class HasMethodClients method repr cts client | cts -> repr, cts -> client where
  getMethodClients :: SProxy method -> Proxy cts -> RequestBuilder -> client

instance hasMethodClientMethodJson
  :: (DecodeJson r, IsSymbol method)
  => HasMethodClients method r JSON (Aff (ajax :: AJAX | e) r) where
  getMethodClients method _ req = do
    r <- toAffjaxRequest req
         # _ { method = toMethod method }
         # affjax
    case decodeJson r.response of
      Left err -> throwError (error err)
      Right x -> pure x

instance hasMethodClientsHTMLString
  :: IsSymbol method
  => HasMethodClients method String HTML (Aff (ajax :: AJAX | e) String) where
  getMethodClients method _ req =
    toAffjaxRequest req
    # _ { method = toMethod method }
    # affjax
    # map _.response

asClients :: forall r mk. HasClients r mk => Proxy r -> mk
asClients = flip getClients emptyRequestBuilder
