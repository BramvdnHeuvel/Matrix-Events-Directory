module Msg exposing (..)

import Element
import Dict exposing (Dict)
import Json.Decode as D
import Http
import Html exposing (s)
import Element.Font exposing (external)
import Html.Attributes exposing (required)

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
    | LoadingFailed Bool String Http.Error
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
    | DoNothing
    | EventReceived String (Result Http.Error Event)
    | SearchEvent String
    | ToggleEventError String
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
            |> D.andThen (\x -> D.field "eventType" (eventTypeDecoder x))
        )
        (D.field "content" objectDecoder)
        ( objectDecoder
            |> D.dict
            |> D.field "objects"
        )
    |> D.andThen checkRequiredDependencies

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
    |> D.field "key"
    |> D.maybe
    |> D.andThen 
        (\k -> 
            D.string
            |> D.field "type"
            |> D.andThen (\t -> identifyObjectType t k)
        )


-- HELPER FUNCTION
identifyObjectType : String -> Maybe String -> D.Decoder ObjectType
identifyObjectType t key =
    if (String.startsWith "[" t && String.endsWith "]" t) then
        t
        |> String.slice 1 -1
        |> (\x -> identifyObjectType x key)
        |> D.map ListOf
    else if (String.startsWith "{" t && String.endsWith "}" t) then
        t
        |> String.slice 1 -1
        |> (\x -> identifyObjectType x key)
        |> D.map DictOf
    else
        case t of
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
                key
                |> Maybe.map External
                |> (\value ->
                    case value of
                        Just v ->
                            D.succeed v
                        Nothing ->
                            D.fail <| "Missing key `key` for unknown object type `" ++ t ++ "`"
                    )

-- CHECK REQUIRED DEPENDENCIES
checkRequiredDependencies : Event -> D.Decoder Event
checkRequiredDependencies event =
    let
        external : ObjectType -> Maybe String
        external ot =
            case ot of
                External o ->
                    Just o
                ListOf o ->
                    external o
                DictOf o ->
                    external o
                _ ->
                    Nothing

        requiredDependencies : String -> List String
        requiredDependencies s =
            case Dict.get s event.otherObjects of
                -- Irrelevant, shouldn't happen
                Nothing ->
                    Dict.keys event.otherObjects
                
                Just o ->
                    o
                    |> List.filter (\field -> field.required)
                    |> List.filterMap (\field -> external field.objectType)
        
        dependencyFinder : String -> List String -> String -> Maybe (List String)
        dependencyFinder start traceback cursor =
            if List.member start (requiredDependencies cursor) then
                Just (traceback ++ [cursor, start])
            else if (requiredDependencies cursor) == [] then
                Nothing
            else
                requiredDependencies cursor
                |> List.map ( dependencyFinder start (traceback ++ [cursor]))
                |> List.filterMap identity
                |> (\x ->
                        case x of
                            [] ->
                                Nothing
                            head :: _ ->
                                Just head
                    )
    in 
        event.otherObjects
        |> Dict.keys
        |> List.map (\k -> dependencyFinder k [] k)
        |> List.filterMap identity
        |> (\x ->
                case x of
                    [] ->
                        D.succeed event
                    head :: _ ->
                        ( "Found a circular dependency of required objects: "
                        ++ String.join " --> " head
                        )
                        |> D.fail
            )
