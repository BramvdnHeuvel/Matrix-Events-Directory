module Msg exposing (..)

import Element
import Dict exposing (Dict)
import Json.Decode as D
import Http
import Html exposing (s)

-- MODEL
type alias Model =
    { device : Element.DeviceClass
    , directory : Maybe Directory
    , events : Dict String EventLoadState
    , eventsLoaded : Int
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
    | LookingAtEvent String Event
    | BrowseEventSetList
    | BrowseEventSet EventSet
    | BrowseEvent EventSet Event
    | About

-- MSG

type Msg
    = ChooseMenuOption Int
    | DirectoryReceived (Result Http.Error Directory)
    | EventReceived String (Result Http.Error Event)
    | SearchEvent String
    | ToggleMenuBar
    | ViewMenu MenuItem
    | WaitThenRequestEvent String Int
    | WindowSize Element.DeviceClass

-- DECODERS

directoryDecoder : D.Decoder Directory
directoryDecoder =
    D.map2
        (\a b -> { all = a, sets = b })
        ( D.string |> D.list |> D.field "all"  )
        ( eventSetDecoder |> D.list |> D.field "sets" )

eventDecoder : D.Decoder Event
eventDecoder =
    D.map5
        (\a b c d e ->
            { name = a, description = b, eventType = c, content = d, otherObjects = e}
        )
        (D.field "name" D.string)
        (D.field "description" D.string)
        ( D.string
            |> D.field "state"
            |> D.maybe
            |> D.andThen eventTypeDecoder
            |> D.field "eventType"
        )
        (D.field "content" objectDecoder)
        ( objectDecoder
            |> D.dict
            |> D.field "otherObjects"
        )

eventSetDecoder : D.Decoder EventSet
eventSetDecoder =
    D.map4
        (\a b c d -> { name = a, description = b, image = c, parts = d})
        (D.field "name" D.string)
        (D.field "description" D.string)
        (D.field "image" D.string)
        (eventSetPartDecoder |> D.list |> D.field "parts")


eventSetPartDecoder : D.Decoder EventSetPart
eventSetPartDecoder =
    D.map3
        (\a b c -> { name = a, description = b, events = c })
        ( D.field "name" D.string)
        ( D.string |> D.list |> D.field "description" )
        ( D.string |> D.list |> D.field "events" )



eventTypeDecoder : Maybe String -> D.Decoder EventType
eventTypeDecoder state =
    D.string
    |> D.andThen
        (\s ->
            case s of
                "message" ->
                    D.succeed Message
                "state" ->
                    case state of
                        Just st ->
                            D.succeed (State st)
                        Nothing ->
                            D.fail "Missing state explanation."
                "ephemeral" ->
                    D.succeed Ephemeral
                _ ->
                    D.fail "Unknown event type"
        )

fieldDecoder : D.Decoder Field
fieldDecoder =
    D.map4
        (\a b c d -> { name = a, description = b, required = c, objectType = d})
        (D.field "name" D.string)
        (D.field "description" D.string)
        (D.field "required" D.bool)
        (objectTypeDecoder)

objectDecoder : D.Decoder Object
objectDecoder =
    D.list fieldDecoder

objectTypeDecoder : D.Decoder ObjectType
objectTypeDecoder =
    D.string
    |> D.field "type"
    |> D.andThen identifyObjectType


-- HELPER FUNCTION
identifyObjectType : String -> D.Decoder ObjectType
identifyObjectType s =
    if (String.startsWith "[" s && String.endsWith "]" s) then
        s
        |> String.slice 1 -1
        |> identifyObjectType
        |> D.map ListOf
    else if (String.startsWith "{" s && String.endsWith "}" s) then
        s
        |> String.slice 1 -1
        |> identifyObjectType
        |> D.map DictOf
    else
        case s of
            "int" ->
                D.succeed Number
            "string" ->
                D.succeed Text
            "enum" ->
                D.string
                |> D.list
                |> D.field "key"
                |> D.map Enum
            _ ->
                External s
                |> D.succeed
