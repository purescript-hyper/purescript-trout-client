module Example.Client where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Hyper.Routing ((:<|>))
import Hyper.Routing.Client (asClients)
import Network.HTTP.Affjax (AJAX)

import Example.Site (site)

main :: forall e. Eff (console :: CONSOLE, ajax :: AJAX, err :: EXCEPTION | e) Unit
main =
  void $ launchAff $ do
    case asClients site of
      _ :<|> (allTasks :<|> getTask) -> do
        log "Getting tasks..."
        allTasks >>= log <<< show

        log "Getting task with ID: 2 ..."
        getTask 2 >>= log <<< show
