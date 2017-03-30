-- Based on https://github.com/alexmingoia/purescript-pux/blob/master/examples/ajax/Todos.purs
module Client where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Hyper.Routing ((:<|>))
import Hyper.Routing.XHR (asClients)
import Network.HTTP.Affjax (AJAX)
import Pux (CoreEffects, EffModel, noEffects, renderToDOM, start)
import Pux.Html (Html, button, div, h1, li, ol, text)
import Pux.Html.Attributes (className, key)
import Pux.Html.Events (onClick)
import Site (Task(..), site)

data Action = RequestTasks
            | ReceiveTasks (Array Task)

type State =
  { tasks :: Array Task
  , status :: String
  }

init :: State
init = { tasks: [], status: "Nothing loaded from server yet" }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (ReceiveTasks tasks) state =
  noEffects $ state { tasks = tasks, status = "Tasks" }
update (RequestTasks) state =
  -- This is where the nice things are going on. Automatically, type-safe
  -- XHR clients. YEY!
  case asClients site of
    allTasks :<|> _ ->
      { state: state { status = "Fetching tasks..." }
      , effects: [ ReceiveTasks <$> allTasks
                 ]
      }

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text state.status ]
    , div
        []
        [ button [ onClick (const RequestTasks) ] [ text "Fetch Tasks" ]
        , ol [] $ map task state.tasks
        ]
    ]

task :: Task -> Html Action
task (Task id title) =
  li [ key (show id), className "task" ] [ text title ]

main :: Eff (CoreEffects (ajax :: AJAX)) Unit
main = do
  app <- start
    { initialState: init
    , update: update
    , view: view
    , inputs: [] }

  renderToDOM "#app" app.html
