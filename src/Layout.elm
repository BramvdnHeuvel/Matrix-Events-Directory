module Layout exposing (..)

import Color exposing (Color, rgb255)
import Element exposing (Element, el)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import Material.Icons.Types as T
import Msg
import Widget.Material exposing (Palette)
import Widget
import Widget.Icon

{-| Attributes of a card
-}
cardAttributes : List (Element.Attribute Msg.Msg)
cardAttributes =
    Widget.Material.cardAttributes primaryPalette ++
    [Background.color <| Element.rgb255 255 255 255] 

{-| Get an icon from the Material library
-}
getIcon : T.Icon Msg.Msg -> Widget.Icon.Icon Msg.Msg
getIcon =
    Widget.Icon.elmMaterialIcons T.Color

{-| Display an icon as a button -}
iconButton : { text : String, icon : T.Icon Msg.Msg, onPress : Maybe Msg.Msg } -> Element Msg.Msg
iconButton data =
    Widget.iconButton
        (Widget.Material.textButton primaryPalette)
        { text = data.text, icon = getIcon data.icon, onPress = data.onPress }

{-| Get a progress loading bar -}
loader : Maybe Float -> Element msg
loader =
    primaryPalette
    |> Widget.Material.progressIndicator
    |> Widget.circularProgressIndicator

-- TEXT CONSTANTS

pageTitle : String
pageTitle = "Matrix events"

-- HEADINGS

h1 : Element msg -> Element msg
h1 = el [ Region.heading 1, Font.size 35 ]

h2 : Element msg -> Element msg
h2 = el [ Region.heading 2, Font.size 28 ]

h3 : Element msg -> Element msg
h3 = el [ Region.heading 3, Font.size 23 ]


bold : Element msg -> Element msg
bold = el [ Font.bold ]

-- PRIMARY COLOR

primaryColor : Color
primaryColor = noordstarBlue

darkPrimaryColor : Color
darkPrimaryColor = rgb255 0x00 0x54 0xbd

lightPrimaryColor : Color
lightPrimaryColor = rgb255 0x80 0xae 0xff

textOnPrimaryColor : Color
textOnPrimaryColor = Color.white

-- SECONDARY COLOR

secondaryColor : Color
secondaryColor = rgb255 0xff 0x8a 0x65

darkSecondaryColor : Color
darkSecondaryColor = rgb255 0xc7 0x5b 0x39

lightSecondaryColor : Color
lightSecondaryColor = rgb255 0xff 0xbb 0x93

textOnSecondaryColor : Color
textOnSecondaryColor = Color.black

-- NOORDSTAR PALETTE

noordstarBlue : Color
noordstarBlue = rgb255 0x42 0x7f 0xf0

noordstarYellow : Color
noordstarYellow = rgb255 0xd2 0xd0 0x24

noordstarGreen : Color
noordstarGreen = rgb255 0x5e 0xa4 0x93

noordstarRed : Color
noordstarRed = rgb255 0xb0 0x00 0x00

noordstarBlack : Color
noordstarBlack = rgb255 0x2c 0x2c 0x48

noordstarWhite : Color
noordstarWhite = rgb255 0xf2 0xef 0xea

-- PALETTES

primaryPalette : Palette
primaryPalette =
    { primary = primaryColor
    , secondary = secondaryColor
    , background = noordstarWhite
    , surface = noordstarWhite
    , error = noordstarRed
    , on =
        { primary = textOnPrimaryColor
        , secondary = textOnSecondaryColor
        , background = Color.black
        , surface = Color.black
        , error = Color.white
        }
    }

darkPalette : Palette
darkPalette =
    { primary = darkPrimaryColor
    , secondary = darkSecondaryColor
    , background = noordstarWhite
    , surface = noordstarWhite
    , error = noordstarRed
    , on =
        { primary = Color.white
        , secondary = Color.white
        , background = Color.black
        , surface = Color.black
        , error = Color.white
        }
    }

lightPalette : Palette
lightPalette =
    { primary = lightPrimaryColor
    , secondary = lightSecondaryColor
    , background = noordstarWhite
    , surface = noordstarWhite
    , error = noordstarRed
    , on =
        { primary = Color.black
        , secondary = Color.black
        , background = Color.black
        , surface = Color.black
        , error = Color.white
        }
    }
