module Example.Client.Main where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign.Object (Object, toUnfoldable)
import JQuery (body, setHtml)
import Text.Smolder.HTML (h1, li, ul)
import Text.Smolder.Markup (Markup, empty, text)
import Text.Smolder.Renderer.String (render)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:/), type (:>))
import Type.Trout.Client (asClients, printError)
import Type.Trout.Client.BaseURI (BaseURI, baseURI)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

newtype BreedList = BreedList (Array (Tuple String (Array String)))

instance decodeJsonBreedList :: DecodeJson BreedList where
  decodeJson json = do
    response :: { message :: Object (Array String) } <- decodeJson json
    pure (BreedList $ toUnfoldable response.message)

breedListMarkup :: forall e. BreedList -> Markup e
breedListMarkup (BreedList breeds) = do
  h1 $ text "Dog Breeds"
  ul $
    for_ breeds \(Tuple breed subBreeds) ->
      li do
        text breed
        case subBreeds of
          [] -> empty
          list -> ul $ for_ list (li <<< text)

type API = BaseURI :> ("breeds" := "breeds" :/ "list" :/ "all" :/ Get BreedList JSON)

main :: Effect Unit
main = do
  case asClients (Proxy :: Proxy API) <$> baseURI "https://dog.ceo/api/" of
    Left error ->
      throw error
    Right { breeds } ->
      launchAff_ do
        bs <- breeds."GET"
        liftEffect $ case bs of
          Left error -> throw $ printError error
          Right breedList ->
            setHtml (render $ breedListMarkup breedList) =<< body
