module Client where

import Prelude hiding (div)
import Control.Monad.Except.Trans (throwError)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import JQuery (body, setHtml)
import Site (site)
import Text.Smolder.Renderer.String (render)
import Type.Trout.Client (asClients, printError)
import Type.Trout.ContentType.HTML (encodeHTML)

main :: Effect Unit
main = do
  b <- body
  void $ launchAff do
    let {tasks} = asClients site
    r <- tasks."GET"
    case r of
      Left err -> throwError (error (printError err))
      Right ts -> liftEffect (setHtml (foldMap (render <<< encodeHTML) ts) b)
