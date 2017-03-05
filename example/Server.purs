module Example.Server where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Array (find, (..))
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType.Common (textHTML)
import Example.Site (Task(..), TaskId, site)
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, contentType, respond, writeStatus)
import Hyper.Routing ((:<|>))
import Hyper.Routing.Router (RoutingError(..), router)
import Hyper.Status (statusNotFound)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.HTTP (HTTP)

type AppM e a = ExceptT RoutingError (ReaderT (Array Task) (Aff e)) a

allTasks :: forall e. AppM e (Array Task)
allTasks = ask

getTask :: forall e. TaskId -> AppM e Task
getTask taskId =
  find (\(Task i _) -> i == taskId) <$> ask >>=
  case _ of
    Just task -> pure task
    Nothing -> throwError (HTTPError { status: statusNotFound
                                     , message: Just "Task not found."
                                     })

main :: forall e. Eff (http :: HTTP, console :: CONSOLE, avar :: AVAR, buffer :: BUFFER, fs :: FS | e) Unit
main =
  runServer' defaultOptionsWithLogging {} (flip runReaderT tasks) siteRouter
  where
    tasks = (map (\i -> Task i ("Task #" <> show i)) (1..10))

    siteRouter = router site (allTasks :<|> getTask) onRoutingError

    notFound =
      writeStatus statusNotFound
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond "<h1>Not Found</h1>"

    onRoutingError status msg
      | status == statusNotFound = fileServer "example/public" notFound

      | otherwise =
        writeStatus status
        :*> contentType textHTML
        :*> closeHeaders
        :*> respond (maybe "" id msg)
