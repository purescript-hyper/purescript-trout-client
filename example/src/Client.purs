module Client where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Data.Foldable (foldMap)
import DOM (DOM)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.JQuery (body, setHtml)
import Network.HTTP.Affjax (AJAX)
import Site (site)
import Text.Smolder.Renderer.String (render)
import Type.Trout.Client (asClients)
import Type.Trout.ContentType.HTML (encodeHTML)

main :: Eff (exception :: EXCEPTION, ajax :: AJAX, dom :: DOM) Unit
main = do
  b <- body
  void $ launchAff do
    let {tasks} = asClients site
    ts <- tasks."GET"
    liftEff (setHtml (foldMap (render <<< encodeHTML) ts) b)
