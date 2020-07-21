module Server where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Indexed ((:*>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Array (find, (..))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Effect (Effect)
import Effect.Aff (Aff)
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Trout.Router (RoutingError(..), router)
import Hyper.Status (statusNotFound)
import Site (Task(..), TaskId, site)

type AppM a = ExceptT RoutingError (ReaderT (Array Task) Aff) a

tasksResource :: {"GET" :: AppM (Array Task)}
tasksResource = {"GET": ask}

taskResource :: TaskId -> {"GET" :: AppM Task}
taskResource taskId =
  {"GET":
   find (\(Task i _) -> i == taskId) <$> ask >>=
   case _ of
     Just task -> pure task
     Nothing -> throwError (HTTPError { status: statusNotFound
                                      , message: Just "Task not found."
                                      })
  }

main :: Effect Unit
main =
  runServer' defaultOptionsWithLogging {} (flip runReaderT tasks) siteRouter
  where
    tasks = (map (\i -> Task i ("Task #" <> show i)) (1..10))
    resources = { task: taskResource
                , tasks: tasksResource
                }
    siteRouter = router site resources onRoutingError

    notFound =
      writeStatus statusNotFound
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond "<h1>Not Found</h1>"

    onRoutingError status msg
      | status == statusNotFound = fileServer "example/client-server/public" notFound

      | otherwise =
        writeStatus status
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond (maybe "" identity msg)
