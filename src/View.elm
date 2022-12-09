module View exposing (..)

import Color
import Element exposing (Element, el, text)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Msg exposing (..)
import Layout
import Search
import Widget
import Widget.Material
import Widget.Material.Color

{-| Create the bar at the top of the page.
-}
topAppBar : Model -> Element Msg
topAppBar model =
    Widget.menuBar
        ( Widget.Material.menuBar Layout.primaryPalette )
        { title = text Layout.pageTitle
                    |> Layout.h1
        , deviceClass = model.device
        , openLeftSheet = Just ToggleMenuBar
        , openTopSheet = Nothing
        , openRightSheet = Nothing
        , primaryActions = []
        , search = Nothing
        }

{-| The main content of the page
-}
mainContent : Model -> Element Msg
mainContent model =
    case model.menu of
        Home ->
            homePage model

        Search s ->
            searchPage model s

        LookingAtEvent s e ->
            Element.none
        
        BrowseEventSetList ->
            Element.none
        
        BrowseEventSet set ->
            Element.none
        
        BrowseEvent set event ->
            Element.none
        
        About ->
            aboutPage

{- REFERENCE FUNCTIONS -}

p : List (Element msg) -> Element msg
p = Element.paragraph []

color : Color.Color -> Element.Attr decorative msg
color = Widget.Material.Color.fromColor >> Font.color

backgroundColor : Color.Color -> Element.Attr decorative msg
backgroundColor = Widget.Material.Color.fromColor >> Background.color

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
                ( "Loading events... (" ++ (String.fromInt model.eventsLoaded)
                ++ "/" ++ (String.fromInt <| List.length d.all) ++ ")"
                )
                |> text
                |> Layout.bold
                |> List.singleton
                |> p
    , if Maybe.map (.all >> List.length) model.directory == Just model.eventsLoaded then
        Element.none
    else
        model.directory
        |> Maybe.map (\_ -> model.eventsLoaded)
        |> Maybe.map toFloat
        |> Layout.loader
    ]
    |> Element.column [ Element.width Element.fill, Element.spacingXY 0 30 ]

searchPage : Model -> String -> Element Msg
searchPage model query =
    [ Input.search [ Element.padding 30 ]
        { onChange = SearchEvent
        , text = query
        , placeholder = Just (Input.placeholder [] (text "Look for events here..."))
        , label = Input.labelHidden "Event lookup search bar"
        }
    , Search.results model query
        |> List.map (\e -> eventPreview (ViewMenu <| LookingAtEvent query e) e)
        |> Element.column []
    ]
    |> Element.column []

aboutPage : Element Msg
aboutPage =
    [ Layout.h2 <| text "About Matrix events"
    , p [ text "Matrix Events is an unofficial registry of Matrix events."
        ]
    , p [ text "As defined by spec, "
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
    , p [ Layout.bold <| text "That will hopefully encourage more interoperability!"]
    ]
    |> Element.column [ Element.width Element.fill, Element.spacingXY 0 30 ]

{- FUNCTIONS THAT RENDER TYPES -}

eventPreview : Msg -> Event -> Element Msg
eventPreview onClick e =
    [ Layout.h2 <| text e.name
    , p [ text e.description ]
    ]
    |> Element.column 
        [ Element.width (Element.fill |> Element.maximum 300)
        , Events.onClick onClick
        ]

