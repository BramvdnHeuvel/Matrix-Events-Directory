module Main exposing (main)

import Browser
import Browser.Events
import Dict
import Element
import Html exposing (Html)
import Layout
import Loader
import Msg exposing (..)
import Random
import View
import Widget.Layout


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( { device = Element.Phone
      , directory = Nothing
      , events = Dict.empty
      , eventsLoaded = 0
      , exampleText = ""
      , exampleMatches = NotDecodedYet
      , menu = Home
      , showMenuBar = False
      , viewportWidth = 200
      }
    , Cmd.batch
        [ Loader.getDirectory
        , Loader.getViewportSize
        ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckExample e ->
            case correctlyDecodesEvent e model.exampleText of
                Ok () ->
                    ( { model | exampleMatches = DecodesProperly }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | exampleMatches = FailesToDecode err }
                    , Cmd.none
                    )

        -- TODO: Write a checker that makes sure the
        --       event is of the expected quality.
        ChooseMenuOption i ->
            case i of
                0 ->
                    ( { model | menu = Home }, Cmd.none )

                1 ->
                    ( { model | menu = Search "" }, Cmd.none )

                2 ->
                    ( { model | menu = BrowseEventSetList }, Cmd.none )

                3 ->
                    ( { model | menu = About }, Cmd.none )

                _ ->
                    update (ChooseMenuOption 0) model

        DirectoryReceived mdir ->
            case mdir of
                Ok d ->
                    ( { model | directory = Just d }
                    , d
                        |> .all
                        |> List.map WaitThenRequestEvent
                        |> List.map
                            (\m ->
                                d.all
                                    |> List.length
                                    |> (*) 500
                                    -- On average, load one event every 500ms
                                    |> Random.int 1
                                    |> Random.generate m
                            )
                        |> Cmd.batch
                    )

                Err _ ->
                    ( model, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )

        EventReceived name event ->
            let
                dictValue : EventLoadState
                dictValue =
                    case event of
                        Ok e ->
                            Loaded e

                        Err e ->
                            LoadingFailed False name e
            in
            ( { model
                | events = Dict.insert name dictValue model.events
                , eventsLoaded =
                    case Dict.get name model.events of
                        Just (Loaded _) ->
                            model.eventsLoaded

                        Just (LoadingFailed _ _ _) ->
                            model.eventsLoaded

                        _ ->
                            model.eventsLoaded + 1
              }
            , Cmd.none
            )

        SearchEvent query ->
            let
                isKnownEvent : Bool
                isKnownEvent =
                    model.directory
                        |> Maybe.map .all
                        |> Maybe.withDefault []
                        |> List.member query

                couldLoad : Bool
                couldLoad =
                    case Dict.get query model.events of
                        Nothing ->
                            False

                        Just Loading ->
                            True

                        Just (Loaded _) ->
                            False

                        Just (LoadingFailed _ _ _) ->
                            True

                offset : Int
                offset =
                    case Dict.get query model.events of
                        Just Loading ->
                            0

                        _ ->
                            -1
            in
            if isKnownEvent && couldLoad then
                ( { model
                    | menu = Search query
                    , showMenuBar = False
                    , events = Dict.insert query Loading model.events
                    , eventsLoaded = model.eventsLoaded + offset
                  }
                , Loader.getEventAfterTimeout 0 query
                )

            else
                ( { model
                    | menu = Search query
                    , showMenuBar = False
                  }
                , Cmd.none
                )

        ToggleEventError name ->
            case Dict.get name model.events of
                Just (LoadingFailed b _ err) ->
                    ( { model
                        | events = Dict.insert name (LoadingFailed (not b) name err) model.events
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleMenuBar ->
            ( { model | showMenuBar = not model.showMenuBar }, Cmd.none )

        ViewMenu menu ->
            ( { model | showMenuBar = False, menu = menu, exampleText = "" }, Cmd.none )

        WaitThenRequestEvent e t ->
            case Dict.get e model.events of
                Nothing ->
                    ( { model
                        | events = Dict.insert e Loading model.events
                      }
                    , Loader.getEventAfterTimeout t e
                    )

                Just Loading ->
                    ( model, Loader.getEventAfterTimeout t e )

                Just (Loaded _) ->
                    ( model, Cmd.none )

                Just (LoadingFailed _ _ _) ->
                    ( { model
                        | eventsLoaded = model.eventsLoaded - 1
                        , events = Dict.insert e Loading model.events
                      }
                    , Loader.getEventAfterTimeout t e
                    )

        WindowSize device ->
            ( { model | device = device }, Cmd.none )

        WindowWidth width ->
            ( { model | viewportWidth = width }, Cmd.none )

        WriteTestEvent content ->
            ( { model | exampleText = content, exampleMatches = NotDecodedYet }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onResize
        (\w h ->
            { width = w, height = h }
                |> Widget.Layout.getDeviceClass
                |> WindowSize
        )
    , Browser.Events.onResize (\w _ -> WindowWidth w)
    ]
        |> Sub.batch



-- VIEW


view : Model -> Html Msg
view model =
    [ View.topAppBar model
    , View.mainContent model
    ]
        |> Element.column [ Element.width Element.fill ]
        |> Element.layout
            ((Layout.noordstarWhite |> View.backgroundColor)
                :: (if model.showMenuBar then
                        View.leftSideBar model

                    else
                        []
                   )
            )
