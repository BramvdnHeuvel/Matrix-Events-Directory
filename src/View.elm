module View exposing (..)

import Color
import Dict
import Element exposing (Element, el, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Http
import Layout
import Material.Icons as Icons
import Msg exposing (..)
import Search
import String.Extra as S
import Widget
import Widget.Customize as Customize
import Widget.Layout
import Widget.Material
import Widget.Material.Color


{-| Create the bar at the top of the page.
-}
topAppBar : Model -> Element Msg
topAppBar model =
    Widget.menuBar
        (Widget.Material.menuBar Layout.primaryPalette)
        { title =
            text Layout.pageTitle
                |> Layout.h1
                |> Element.el [ color Color.white ]
        , deviceClass = model.device
        , openLeftSheet = Just ToggleMenuBar
        , openTopSheet = Nothing
        , openRightSheet = Nothing
        , primaryActions = []
        , search = Nothing
        }


{-| Sidebar on the left that pops up when you view the menu
-}
leftSideBar : Model -> List (Element.Attribute Msg)
leftSideBar model =
    { title = el [ Element.padding 15 ] (Layout.h2 <| text "Matrix events")
    , onDismiss = ToggleMenuBar
    , menu =
        { selected =
            Just <|
                case model.menu of
                    Home ->
                        0

                    Search _ ->
                        1

                    LookingAtEvent _ _ ->
                        1

                    BrowseEventSetList ->
                        2

                    BrowseEventSet _ ->
                        2

                    BrowseEvent _ _ ->
                        2

                    About ->
                        3
        , options =
            [ { text = "Home"
              , icon = Layout.getIcon Icons.home
              }
            , { text = "Search"
              , icon = Layout.getIcon Icons.search
              }
            , { text = "Browse"
              , icon = Layout.getIcon Icons.view_list
              }
            , { text = "About"
              , icon = Layout.getIcon Icons.description
              }
            ]
        , onSelect =
            \i ->
                (case i of
                    0 ->
                        Just Home

                    1 ->
                        Just (Search "")

                    2 ->
                        Just BrowseEventSetList

                    3 ->
                        Just About

                    _ ->
                        Nothing
                )
                    |> Maybe.map ViewMenu
        }
    }
        |> Widget.Layout.leftSheet
            { button = Widget.Material.selectItem Layout.primaryPalette
            , sheet = Widget.Material.sideSheet Layout.primaryPalette
            }
        |> List.singleton
        |> Widget.singleModal


{-| The main content of the page
-}
mainContent : Model -> Element Msg
mainContent model =
    (case model.menu of
        Home ->
            homePage model

        Search s ->
            searchPage model s

        LookingAtEvent s e ->
            let
                search : String
                search =
                    " \"" ++ (S.ellipsis 20 <| S.softEllipsis 15 s) ++ "\""
            in
            [ navigationBar
                [ ( "Search"
                        ++ (case search of
                                " \"\"" ->
                                    ""

                                _ ->
                                    search
                           )
                  , Search s
                  )
                , ( e.name, LookingAtEvent s e )
                ]
            , showEvent model e
            ]
                |> Element.column [ Element.centerX ]

        BrowseEventSetList ->
            browsePage model

        BrowseEventSet set ->
            [ navigationBar
                [ ( "Browse", BrowseEventSetList )
                , ( set.name, BrowseEventSet set )
                ]
            , showEventSet model set
            ]
                |> Element.column [ Element.centerX ]

        BrowseEvent set evt ->
            [ navigationBar
                [ ( "Browse", BrowseEventSetList )
                , ( set.name, BrowseEventSet set )
                , ( evt.name, BrowseEvent set evt )
                ]
            , showEvent model evt
            ]
                |> Element.column []

        About ->
            aboutPage
    )
        |> Element.el
            [ Element.padding 30
            , Element.width (Element.fill |> Element.maximum 1250)
            , Element.centerX
            ]



{- REFERENCE FUNCTIONS -}


p : List (Element msg) -> Element msg
p =
    Element.paragraph []


color : Color.Color -> Element.Attr decorative msg
color =
    Widget.Material.Color.fromColor >> Font.color


backgroundColor : Color.Color -> Element.Attr decorative msg
backgroundColor =
    Widget.Material.Color.fromColor >> Background.color



{- PAGE FUNCTIONS -}


homePage : Model -> Element Msg
homePage model =
    [ Layout.h2 <| text "Matrix Events Home"
    , p [ text "Matrix Events is an unofficial registry of Matrix events." ]
    , case model.directory of
        Nothing ->
            "Loading event directory..."
                |> text
                |> Layout.bold
                |> List.singleton
                |> p

        Just d ->
            if List.length d.all == model.eventsLoaded then
                Element.none

            else
                ("Loading events... ("
                    ++ String.fromInt model.eventsLoaded
                    ++ "/"
                    ++ (String.fromInt <| List.length d.all)
                    ++ ")"
                )
                    |> text
                    |> Layout.bold
                    |> List.singleton
                    |> p
    , if Maybe.map (.all >> List.length) model.directory == Just model.eventsLoaded then
        model.events
            |> Dict.values
            |> List.map failedEvent
            |> Element.column [ Element.width (Element.fill |> Element.maximum 500), Element.centerX ]

      else
        model.directory
            |> Maybe.map (.all >> List.length >> toFloat)
            |> Maybe.map (\d -> toFloat model.eventsLoaded / d)
            |> Layout.loader
    ]
        |> Element.column [ Element.width Element.fill, Element.spacingXY 0 30 ]


searchPage : Model -> String -> Element Msg
searchPage model query =
    [ Input.search
        [ Element.padding 30
        , Element.width Element.fill
        ]
        { onChange = SearchEvent
        , text = query
        , placeholder = Just (Input.placeholder [] (text "Look for events here..."))
        , label = Input.labelHidden "Event lookup search bar"
        }
    , Search.results model query
        |> List.map
            (\e ->
                eventPreview
                    (case e of
                        Loaded event ->
                            ViewMenu <| LookingAtEvent query event

                        _ ->
                            DoNothing
                    )
                    e
            )
        |> Element.column [ Element.width Element.fill ]
    ]
        |> Element.column [ Element.width Element.fill, Element.spacing 50 ]


browsePage : Model -> Element Msg
browsePage model =
    case model.directory of
        Nothing ->
            Element.none

        Just dir ->
            dir.sets
                |> List.map eventSetPreview
                |> Element.wrappedRow [ Element.spacing 10 ]


aboutPage : Element Msg
aboutPage =
    [ Layout.h2 <| text "About Matrix events"
    , p
        [ text "Matrix Events is an unofficial registry of Matrix events."
        ]
    , p
        [ text "As defined by spec, "
        , Element.link [ color Layout.darkPrimaryColor ]
            { url = "https://spec.matrix.org/v1.5/client-server-api/#events"
            , label = text "events are not limited to the types defined in the Matrix specification."
            }
        , text " New or custom event types can be created on a whim using the Java package naming convention."
        ]
    , p [ text "There is no official registry of custom event types, however, and this leads to several problems:" ]
    , p [ el [ Element.paddingXY 10 0 ] <| text "1. Clients will encounter events that they do not know, and have no way of looking up." ]
    , p [ el [ Element.paddingXY 10 0 ] <| text "2. Different groups of client developers are encouraged to generate their own event types and event content. This disencourages interoperability among different clients." ]
    , p [ text "Ultimately, this will result in many developers reinventing the wheel and then getting frustrated that most clients cannot support their events." ]
    , p [ text "This website aims to form an unofficial registry of event types. That way, client developers can look up non-spec events that they see appear often and consider to add compatibility for. It also means they can look up existing events and build with those, so that separate clients may already be compatible with their ideas." ]
    , p [ Layout.bold <| text "That will hopefully encourage more interoperability!" ]
    ]
        |> Element.column [ Element.width Element.fill, Element.spacingXY 0 30 ]



{- Navigation -}


navigationBar : List ( String, MenuItem ) -> Element Msg
navigationBar items =
    items
        |> List.map
            (\( item, m ) ->
                item
                    |> text
                    |> Layout.h3
                    |> Element.el
                        (Layout.cardAttributes
                            ++ [ Events.onClick <| ViewMenu m
                               , Element.width Element.shrink
                               ]
                        )
            )
        |> List.intersperse (Layout.h3 (text ">"))
        |> Element.row
            [ Element.spacing 20
            , Element.paddingEach
                { top = 0, left = 30, right = 30, bottom = 30 }
            ]



{- List of objects that didn't parse -}


failedEvent : EventLoadState -> Element Msg
failedEvent state =
    case state of
        LoadingFailed opened name err ->
            if not opened then
                Element.column
                    (Layout.cardAttributes ++ [ Element.centerX ])
                    [ Element.row
                        [ Element.width Element.fill, Events.onClick (ToggleEventError name) ]
                        [ el [ Element.alignLeft ] (text name)
                        , Layout.iconButton
                            { text = "Close error"
                            , icon = Icons.keyboard_arrow_up
                            , onPress = Just DoNothing
                            }
                            |> el [ Element.alignRight ]
                        ]
                    , Element.column []
                        []
                    ]

            else
                Element.column
                    (Layout.cardAttributes ++ [ Element.centerX ])
                    [ Element.row
                        [ Element.width Element.fill, Events.onClick (ToggleEventError name) ]
                        [ el [ Element.alignLeft ] (text name)
                        , Layout.iconButton
                            { text = "Open error"
                            , icon = Icons.keyboard_arrow_down
                            , onPress = Just DoNothing
                            }
                            |> el [ Element.alignRight ]
                        ]
                    , Element.column [ Element.padding 10 ]
                        [ (case err of
                            Http.BadBody body ->
                                body

                            Http.NetworkError ->
                                "Couldn't download the file. Did you turn off WiFi or enter a cave?"

                            Http.Timeout ->
                                "The server took too long to respond."

                            Http.BadStatus 404 ->
                                "ERROR: 404 NOT FOUND. (Maybe the file doesn't exist?)"

                            Http.BadStatus 403 ->
                                "ERROR: 403 FORBIDDEN. Maybe you downloaded the events too quickly."

                            Http.BadStatus 500 ->
                                "ERROR: 500 INTERNAL ERROR. It seems like the server is having some issues. :/"

                            Http.BadStatus statusCode ->
                                "Received status code " ++ String.fromInt statusCode ++ " from the server."

                            Http.BadUrl url ->
                                "The URL `" ++ url ++ "` is not valid."
                          )
                            |> text
                            |> el [ Font.family [ Font.monospace ], color Layout.noordstarRed ]
                            |> List.singleton
                            |> p
                        ]
                    ]

        _ ->
            Element.none



{- FUNCTIONS THAT RENDER TYPES -}


eventPreview : Msg -> EventLoadState -> Element Msg
eventPreview onClick event =
    (case event of
        Loaded e ->
            [ Layout.h2 <| text e.name
            , p [ text e.description ]
            ]

        Loading ->
            [ Layout.h2 <| text "Event is loading..."
            , Layout.loader Nothing
            ]

        LoadingFailed _ name _ ->
            [ [ Layout.h2 <| text name
              , Widget.iconButton
                    (Widget.Material.containedButton Layout.secondaryPalette)
                    { text = "Retry"
                    , icon = Layout.getIcon Icons.refresh
                    , onPress = Just (WaitThenRequestEvent name 0)
                    }
              ]
                |> Element.row [ Element.spaceEvenly, Element.width Element.fill ]
            , p [ text "Loading failed. Please try again." ]
            ]
    )
        |> Element.column
            (Layout.cardAttributes
                ++ [ Element.width Element.fill
                   , Events.onClick onClick
                   , Element.centerX
                   ]
            )


showEvent : Model -> Event -> Element Msg
showEvent model event =
    [ [ Layout.h1 <| text event.name
      , p [ text event.description ]
      , objectTable event.content
      ]
        |> Element.column [ Element.spacing 30 ]
    , event.otherObjects
        |> Dict.toList
        |> List.map
            (\( name, o ) ->
                [ Layout.h2 <| text name
                , objectTable o
                ]
            )
        |> List.map (Element.column [])
        |> Element.column [ Element.spacing 30 ]
    , [ Input.multiline
            (Font.family [ Font.monospace ]
                :: (case model.exampleMatches of
                        DecodesProperly ->
                            [ backgroundColor Layout.noordstarGreen ]

                        FailesToDecode _ ->
                            [ backgroundColor Layout.noordstarRed ]

                        _ ->
                            []
                   )
            )
            { onChange = WriteTestEvent
            , text = model.exampleText
            , placeholder = text "Write a test object!" |> Input.placeholder [] |> Just
            , label = Input.labelAbove [] (Layout.h2 <| text "Test object")
            , spellcheck = False
            }
      , case model.exampleMatches of
            NotDecodedYet ->
                Widget.textButton
                    (Widget.Material.containedButton Layout.secondaryPalette)
                    { text = "Check", onPress = Just (CheckExample event) }

            FailesToDecode err ->
                err
                    |> text
                    |> List.singleton
                    |> p
                    |> Element.el [ Font.family [ Font.monospace ], color Layout.noordstarRed ]

            DecodesProperly ->
                "Valid event!"
                    |> text
                    |> List.singleton
                    |> p
                    |> Element.el [ Font.family [ Font.monospace ], color Layout.noordstarGreen ]
      , showExamples event (codeBlock model)
      ]
        |> Element.column [ Element.width Element.fill ]
    ]
        |> Element.column
            (Layout.cardAttributes
                ++ [ Element.spacing 100
                   , Element.paddingEach { top = 50, left = 50, right = 50, bottom = 100 }
                   ]
            )


eventSetPreview : EventSet -> Element Msg
eventSetPreview set =
    [ Element.el [ Element.centerX ] <| Layout.h1 <| text set.name
    , Element.image
        [ Element.centerX
        , Element.width (Element.px 150)
        , Element.height (Element.px 150)
        , Border.rounded 75
        , Element.clip
        ]
        { src = "/images/" ++ set.image
        , description = set.name ++ " image"
        }
    , text set.description
        |> List.singleton
        |> p
        |> Element.el [ Element.centerX, Element.centerY ]
    ]
        |> Element.column
            (Layout.cardAttributes
                ++ [ BrowseEventSet set
                        |> ViewMenu
                        |> Events.onClick
                   , Element.height (Element.px 350)
                   ]
            )


showEventSet : Model -> EventSet -> Element Msg
showEventSet model set =
    [ [ Layout.h1 <| text set.name
      , p [ text set.description ]
      ]
        |> Element.column [ Element.spacing 30 ]
    , set.parts
        |> List.map
            (\part ->
                [ Layout.h2 <| text part.name
                , part.description
                    |> List.map text
                    |> List.map List.singleton
                    |> List.map p
                    |> Element.column [ Element.spacing 30 ]
                , part.events
                    |> List.map (\e -> Dict.get e model.events)
                    |> List.filterMap identity
                    |> List.map
                        (\e ->
                            eventPreview
                                (case e of
                                    Loaded event ->
                                        ViewMenu <| BrowseEvent set event

                                    _ ->
                                        DoNothing
                                )
                                e
                        )
                    |> Element.column [ Element.width Element.fill, Element.spacing 30 ]
                ]
                    |> Element.column [ Element.spacing 30, Element.width Element.fill ]
            )
        |> Element.column [ Element.spacing 80, Element.width Element.fill ]
    ]
        |> Element.column (Layout.cardAttributes ++ [ Element.spacing 80 ])


objectTable : Object -> Element Msg
objectTable o =
    Widget.sortTableV2
        (Widget.Material.sortTable Layout.secondaryPalette
            |> Customize.elementTable [ Element.spacing 20, Element.width Element.fill ]
        )
        { content = o
        , columns =
            [ Widget.unsortableColumnV2
                { title = "Field"
                , toString = .name
                , width = Element.shrink
                }
            , Widget.unsortableColumnV2
                { title = "Type"
                , toString = .objectType >> fromObjectType
                , width = Element.shrink
                }
            , Widget.customColumnV2
                { title = "Required"
                , value =
                    \{ required } ->
                        Widget.iconButton
                            (Widget.Material.iconButton Layout.secondaryPalette)
                            { text = "Required"
                            , icon =
                                (if required then
                                    Icons.check

                                 else
                                    Icons.cancel
                                )
                                    |> Layout.getIcon
                            , onPress = Just DoNothing
                            }
                            |> Element.el [ Element.centerX ]
                , width = Element.shrink
                }
            , Widget.unsortableColumnV2
                { title = "Description"
                , toString = .description
                , width = Element.fill
                }
            ]
        , sortBy = "name"
        , asc = True
        , onChange = \_ -> DoNothing
        }
        |> List.singleton
        |> p


showExamples : Event -> (String -> Element Msg) -> Element Msg
showExamples event codeMaker =
    event.examples
        |> List.map
            (\example ->
                [ Layout.h2 <| text example.name
                , p <| [ text example.description ]
                , example.value
                    |> codeMaker

                -- |> List.singleton
                -- |> Element.column
                --     ( Layout.cardAttributes
                --     ++ [ Font.family [ Font.monospace ]
                --        , backgroundColor Layout.noordstarWhite
                --        , Element.width Element.fill
                --        , example.value
                --         |> String.split "\n"
                --         |> List.length
                --         |> (\x ->
                --                 if x < 5 then
                --                     x * 40
                --                 else
                --                     x * 32
                --             )
                --         |> Element.px
                --         |> Element.maximum 1000
                --         |> Element.height
                --        , Element.clipX
                --        , Element.scrollbarX
                --        , Element.clipY
                --        , Element.scrollbarY
                --        ]
                --     )
                ]
                    |> Element.column
                        [ Element.width Element.fill
                        ]
            )
        |> Element.column
            [ Element.paddingXY 0 50
            , Element.spacing 20
            ]


codeBlock : Model -> String -> Element Msg
codeBlock model code =
    -- https://github.com/mdgriffith/elm-ui/issues/321
    Element.row [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.el
            [ Element.width (Element.maximum (model.viewportWidth - 150) Element.fill)
            , Element.scrollbarX
            , Background.color (Element.rgba 0 0 0 0.1)
            , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
            , Element.padding 10
            , Font.family [ Font.monospace ]
            , Border.rounded 15
            ]
            (Element.text code)
        ]
