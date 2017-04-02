-- Based on https://github.com/alexmingoia/purescript-pux/blob/master/examples/ajax/Todos.purs
module Client where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Foldable (for_)
import Hyper.Routing ((:<|>))
import Hyper.Routing.XHR (asClients)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, CoreEffects, start)
import Pux.Renderer.React (renderToDOM)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (li, div, h1, button, ol)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!), (#!), text)
import Site (Task(..), site)

data Event = RequestTasks
           | ReceiveTasks (Array Task)

type State =
  { tasks :: Array Task
  , status :: String
  }

init :: State
init = { tasks: [], status: "Nothing loaded from server yet" }

foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)
foldp (ReceiveTasks tasks) state =
  noEffects $ state { tasks = tasks, status = "Tasks" }
foldp (RequestTasks) state =
  -- This is where the nice things are going on. Automatically, type-safe
  -- XHR clients. YEY!
  case asClients site of
    allTasks :<|> _ ->
      { state: state { status = "Fetching tasks..." }
      , effects: [ (Just <<< ReceiveTasks) <$> allTasks
                 ]
      }

view :: State -> HTML Event
view state =
  div do
    h1 $ text state.status
    div do
      button #! onClick (const RequestTasks) $ text "Fetch Tasks"
      ol $ for_ state.tasks task

task :: Task -> HTML Event
task (Task id title) =
  li ! key (show id) ! className "task" $ text title

main :: Eff (CoreEffects (ajax :: AJAX)) Unit
main = do
  app <- start
    { initialState: init
    , foldp
    , view
    , inputs: [] }

  renderToDOM "#app" app.markup app.input
