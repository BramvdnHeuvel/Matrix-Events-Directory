module Search exposing (..)

import Dict
import Msg exposing (..)
import String.Extra as S


results : Model -> String -> List EventLoadState
results model query =
    let
        score : String -> Int
        score =
            breakOpen >> likeness (breakOpen query)
    in
    model.events
        |> Dict.toList
        |> List.sortBy
            (\( name, eventLoadState ) ->
                case eventLoadState of
                    Loaded e ->
                        [ score e.name
                        , score e.description
                        , e.otherObjects
                            |> Dict.keys
                            |> List.map score
                            |> List.maximum
                            |> Maybe.withDefault 0
                        , e.otherObjects
                            |> Dict.values
                            |> List.map (List.map (.description >> score))
                            |> List.filterMap List.maximum
                            |> List.maximum
                            |> Maybe.withDefault 0
                        , e.otherObjects
                            |> Dict.values
                            |> List.map (List.map (.name >> score))
                            |> List.filterMap List.maximum
                            |> List.maximum
                            |> Maybe.withDefault 0
                        ]
                            |> List.map (\x -> -1 * x)

                    _ ->
                        [ score name
                        , 0
                        , 0
                        , 0
                        , 0
                        ]
                            |> List.map (\x -> -1 * x)
            )
        |> List.map Tuple.second


likeness : List String -> List String -> Int
likeness query story =
    query
        |> List.map (\q -> List.member q story)
        |> List.filter identity
        |> List.length


{-| Break words up so it becomes easier to search for them.
-}
breakOpen : String -> List String
breakOpen =
    S.humanize
        >> String.toLower
        >> S.removeAccents
        >> String.replace "." " "
        >> String.replace "." " "
        >> String.replace "," " "
        >> String.replace "-" " "
        >> String.replace ":" " "
        >> String.replace ";" " "
        >> String.replace "!" " "
        >> String.replace "?" " "
        >> String.replace "@" " "
        >> String.replace "-" " "
        >> String.replace "_" " "
        >> String.replace "+" " "
        >> String.replace "&" " "
        >> String.replace "^" " "
        >> String.replace "#" " "
        >> String.replace "~" " "
        >> String.replace "`" " "
        >> String.split " "
