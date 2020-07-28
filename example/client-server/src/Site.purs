module Example.ClientServer.Site where

import Prelude
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Text.Smolder.HTML (h2, p)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, Resource)
import Type.Trout.ContentType.HTML (class EncodeHTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

type TaskId = Int

data Task = Task TaskId String

derive instance genericTask :: Generic Task _

instance showTask :: Show Task where show = genericShow
instance encodeJsonTask :: EncodeJson Task where encodeJson = genericEncodeJson
instance decodeJsonTask :: DecodeJson Task where decodeJson = genericDecodeJson

instance encodeHTMLTask :: EncodeHTML Task where
  encodeHTML (Task id' description) = do
    h2 (text ("Task " <> show id'))
    p (text description)

type TasksResource = Resource (Get (Array Task) JSON)

type TaskResource = Resource (Get Task JSON)

type Site =
  "tasks" :/ (     "tasks" := TasksResource
              :<|> "task"  := Capture "id" TaskId :> TaskResource)

site :: Proxy Site
site = Proxy
