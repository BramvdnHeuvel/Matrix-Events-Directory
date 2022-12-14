module Loader exposing (..)

{-| The Loader module loads all JSON files from the `content/` folder.
-}

import Browser.Dom as Dom
import Dict
import Http
import Json.Decode as D
import Msg exposing (..)
import Process
import Task
import Widget.Layout


{-| Get the contents of `dir.json` as a directory for all events.
-}
getDirectory : Cmd Msg
getDirectory =
    Http.get
        { url = "/events/dir.json"
        , expect = Http.expectJson DirectoryReceived directoryDecoder
        }


{-| Get an event. To prevent a user from spamming the API with a boatload of JSON files,
load each event at a slightly later moment in time.
-}
getEventAfterTimeout : Int -> String -> Cmd Msg
getEventAfterTimeout timeout eventType =
    timeout
        |> toFloat
        |> Process.sleep
        |> Task.andThen
            (\_ ->
                Http.task
                    { method = "GET"
                    , headers = []
                    , url = "/events/" ++ eventType ++ ".json"
                    , body = Http.emptyBody
                    , resolver = Http.stringResolver jsonResolver
                    , timeout = Nothing
                    }
            )
        |> Task.attempt (EventReceived eventType)


maybeGetEvent : Model -> String -> Maybe (Cmd Msg)
maybeGetEvent model name =
    case Dict.get name model.events of
        Just Loading ->
            Nothing

        Just (Loaded _) ->
            Nothing

        Just (LoadingFailed _ _ _) ->
            Just <| getEventAfterTimeout 0 name

        Nothing ->
            Just <| getEventAfterTimeout 0 name


{-| Get the size of the viewport.
-}
getViewportSize : Cmd Msg
getViewportSize =
    [ Dom.getViewport
        |> Task.map
            (\window ->
                { height = round window.viewport.height
                , width = round window.viewport.width
                }
            )
        |> Task.map Widget.Layout.getDeviceClass
        |> Task.perform WindowSize
    , Dom.getViewport
        |> Task.map
            (\window -> round window.viewport.width)
        |> Task.perform WindowWidth
    ]
    |> Cmd.batch


{-| Decode an event from a HTTP response string.
-}
jsonResolver : Http.Response String -> Result Http.Error Event
jsonResolver resp =
    case resp of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case D.decodeString eventDecoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (D.errorToString err))
