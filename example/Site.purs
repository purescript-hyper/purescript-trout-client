module Example.Site where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, gDecodeJson, gEncodeJson)
import Data.Generic (class Generic, gShow)
import Data.Tuple (Tuple(..))
import Hyper.HTML (class EncodeHTML, HTML, element)
import Hyper.Routing (type (:/), type (:<|>), type (:>), Capture)
import Hyper.Routing.Method (Get)
import Type.Proxy (Proxy(..))

data Home = Home

instance encodeHTMLPost :: EncodeHTML Home where
  encodeHTML _ =
    element "div" [ Tuple "id" "main" ] [ element "script" [ Tuple "src" "bundle.js" ] []]

type TaskId = Int

data Task = Task TaskId String

derive instance genericTask :: Generic Task

instance showTask :: Show Task where show = gShow
instance encodeJsonTask :: EncodeJson Task where encodeJson = gEncodeJson
instance decodeJsonTask :: DecodeJson Task where decodeJson = gDecodeJson

type Site =
  Get HTML Home
  :<|> "tasks" :/ (Get Json (Array Task)
                   :<|> Capture "id" TaskId :> Get Json Task)

site :: Proxy Site
site = Proxy
