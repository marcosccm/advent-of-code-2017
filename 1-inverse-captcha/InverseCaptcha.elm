module InverseCaptcha exposing (..)

import Array


calculate : String -> Int
calculate entry =
    let
        chars =
            String.split "" entry
    in
        List.map2 keepIfMatch chars (shiftFirstToLast chars)
            |> List.filterMap identity
            |> List.map (String.toInt >> Result.toMaybe >> Maybe.withDefault 0)
            |> List.sum


calculate2 : String -> Int
calculate2 entry =
    let
        chars =
            String.split "" entry

        halfPoint =
            (List.length chars |> toFloat) / 2 |> round

        fullLength =
            List.length chars

        entryArray =
            Array.fromList chars

        halfwayList =
            List.indexedMap
                (\pos _ ->
                    Array.get ((pos + halfPoint) % fullLength) entryArray
                )
                chars
                |> List.filterMap identity
    in
        List.map2 keepIfMatch chars halfwayList
            |> List.filterMap identity
            |> List.map (String.toInt >> Result.toMaybe >> Maybe.withDefault 0)
            |> List.sum


keepIfMatch : a -> a -> Maybe a
keepIfMatch a b =
    if a == b then
        Just a
    else
        Nothing


shiftFirstToLast : List a -> List a
shiftFirstToLast a =
    case a of
        [] ->
            []

        x :: xs ->
            xs ++ [ x ]
