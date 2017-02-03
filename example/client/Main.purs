module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?))
import Hyper.Routing (type (:/), type (:<|>), type (:>), CaptureAll, (:<|>))
import Hyper.Routing.Client (asClients)
import Hyper.Routing.Method (Get)
import Network.HTTP.Affjax (AJAX)
import Type.Proxy (Proxy(..))

data Task = Task String

instance showTask :: Show Task where
  show (Task name) = "Task: " <> name

instance decodeJsonTask :: DecodeJson Task where
  decodeJson json = do
    o <- decodeJson json
    name <- o .? "name"
    pure (Task name)

type Site = "example" :/ "tasks.json" :/ Get Json (Array Task)
            :<|> "src" :/ CaptureAll "segment" String :> Get String String

site :: Proxy Site
site = Proxy

main :: forall e. Eff (console :: CONSOLE, ajax :: AJAX, err :: EXCEPTION | e) Unit
main =
  void $ launchAff $ do
    case asClients site of
      tasks :<|> srcFile -> do
        log "Getting tasks..."
        tasks >>= log <<< show

        log "Getting source file..."
        srcFile ["Hyper", "Routing", "Client.purs"] >>= log
