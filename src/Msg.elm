module Msg exposing (..)

import Element
import Dict exposing (Dict)
import Json.Decode exposing (Error(..))
import Http

-- MODEL
type alias Model =
    { device : Element.DeviceClass
    , directory : Maybe Directory
    , events : Dict String EventLoadState
    , menu : MenuItem
    , showMenuBar : Bool
    }

type alias Directory =
    { all : List String
    , sets : List EventSet
    }

type alias EventSet =
    { name : String
    , description : String
    , image : String
    , parts : List EventSetPart
    }

type alias EventSetPart =
    { name : String
    , description : List String
    , events : List String
    }

type EventLoadState
    = Loading
    | LoadingFailed Http.Error
    | Loaded Event

type alias Event =
    { name : String
    , description : String
    , eventType : EventType
    , content : Object
    , otherObjects : Dict String Object
    }

type EventType
    = Message
    | State String
    | Ephemeral

type alias Object = List Field

type alias Field =
    { name : String
    , description : String
    , required : Bool
    , objectType : ObjectType
    }

type ObjectType
    = Text
    | Number
    | Enum (List String)
    | ListOf ObjectType
    | DictOf ObjectType
    | External String

type MenuItem
    = Home
    | Search String
    | LookingAtEvent Event
    | BrowseEventSetList
    | BrowseEventSet EventSet
    | BrowseEvent EventSet Event
    | About

-- MSG

type Msg
    = WindowSize Element.DeviceClass
    | ToggleMenuBar
    | ChooseMenuOption Int
    | DirectoryReceived (Result Http.Error Directory)
    | EventReceived (Result Http.Error Event)
    | WaitThenRequestEvent String Int
    | ViewMenu MenuItem
