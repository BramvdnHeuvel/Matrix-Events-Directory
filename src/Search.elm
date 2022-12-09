module Search exposing (..)

import Msg exposing (..)
import Dict

results : Model -> String -> List Event
results model query =
    let
        score : String -> Int
        score = likeness query
    in
        model.events
        |> Dict.values
        |> List.filterMap
            (\eventLoadState ->
                case eventLoadState of
                    Loaded e ->
                        Just e
                    _ ->
                        Nothing
            )
        |> List.sortBy
            (\e ->
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
            )

likeness : String -> String -> Int
likeness sa sb =
    let
        {- Punishment for every wrong character -}
        p : Int
        p = -1

        {- Reward for every matching character -}
        reward : Int
        reward = 100
    in
    case (String.toList sa, String.toList sb) of
        (_, []) ->
            p * (String.length sa)
        
        ([], _) ->
            p * (String.length sb)
        
        (a :: ta, b :: tb) ->
            if a == b then
                ( likeness 
                    (String.fromList ta) 
                    (String.fromList tb)
                ) + reward
            else
                ( max
                    (likeness sa (String.fromList tb))
                    (likeness sb (String.fromList ta))
                ) + p

