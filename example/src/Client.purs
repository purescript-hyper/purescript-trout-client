module Client where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldMap)
import DOM (DOM)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.JQuery (body, setHtml)
import Network.HTTP.Affjax (AJAX)
import Site (Task(..), site)
import Text.Smolder.HTML (li, div, h1, button, ol)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!), (#!), text)
import Text.Smolder.Renderer.String (render)
import Type.Trout ((:<|>))
import Type.Trout.Client (asClients)
import Type.Trout.ContentType.HTML (encodeHTML)

main :: Eff (exception :: EXCEPTION, ajax :: AJAX, dom :: DOM) Unit
main = do
  b <- body
  void $ launchAff $
    case asClients site of
      allTasks :<|> _ -> do
        tasks <- allTasks
        liftEff (setHtml (foldMap (render <<< encodeHTML) tasks) b)

