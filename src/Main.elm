module Main exposing (main)


import Browser
import Browser.Events
import Dict
import Element
import Html exposing (Html)
import Layout
import Loader
import Msg exposing (..)
import View
import Widget.Layout
import List exposing (all)

main = Browser.element
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
      , menu = Home
      , showMenuBar = False
      }
    , Cmd.batch
        [ Loader.getDirectory
        , Loader.getViewportSize
        ]
    )

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
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
                    , Cmd.batch
                        []
                    )
                
                Err _ ->
                    ( model, Cmd.none )
        
        EventReceived name event ->
            case event of
                Ok e ->
                    ( { model | events = Dict.insert name (Loaded e) model.events }
                    , Cmd.none
                    )
                
                Err e ->
                    ( { model | events = Dict.insert name (LoadingFailed e) model.events }
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
            in
            ( { model 
              | menu = Search query
              , showMenuBar = False
              }
            , if isKnownEvent then
                Cmd.none -- TODO: If it is a known event that has not been
                         --       downloaded yet, make sure to download it
                         --       right away.
            else
                Cmd.none
            )

        ToggleMenuBar ->
            ( { model | showMenuBar = not model.showMenuBar }, Cmd.none )
        
        ViewMenu menu ->
            ( { model | menu = menu }, Cmd.none )
        
        WaitThenRequestEvent e t ->
            ( model, Loader.getEventAfterTimeout t e )
        
        WindowSize device ->
            ( { model | device = device }, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize
        (\w h -> 
            { width = w, height = h}
            |> Widget.Layout.getDeviceClass
            |> WindowSize
        )

-- VIEW
view : Model -> Html Msg
view model =
    [ View.topAppBar model
    , View.mainContent model
    ]
    |> Element.column [ Element.width Element.fill ]
    |> Element.layout
        ( (Layout.noordstarWhite |> View.backgroundColor ) ::
            (if model.showMenuBar then
                [] -- View.leftSideBar model
            else
                []
            )
        )



