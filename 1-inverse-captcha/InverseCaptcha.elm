module InverseCaptcha exposing (..)


calculate : String -> Int
calculate entry =
    let
        match a b =
            if a == b then
                Just a
            else
                Nothing

        chars =
            String.split "" entry
    in
        List.map2 match chars (shiftFirstToLast chars)
            |> List.filterMap identity
            |> List.map (String.toInt >> Result.toMaybe >> Maybe.withDefault 0)
            |> List.sum


shiftFirstToLast : List a -> List a
shiftFirstToLast a =
    case a of
        [] ->
            []

        x :: xs ->
            xs ++ [ x ]
