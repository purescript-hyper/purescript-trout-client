module Client where

import Prelude hiding (div)
import Data.Foldable (foldMap)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import JQuery (body, setHtml)
import Site (site)
import Text.Smolder.Renderer.String (render)
import Type.Trout.Client (asClients)
import Type.Trout.ContentType.HTML (encodeHTML)

main :: Effect Unit
main = do
  b <- body
  void $ launchAff do
    let {tasks} = asClients site
    ts <- tasks."GET"
    liftEffect (setHtml (foldMap (render <<< encodeHTML) ts) b)
