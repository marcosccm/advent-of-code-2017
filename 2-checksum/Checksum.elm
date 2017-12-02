module Checksum exposing (calculate, calculate2)


calculate : List (List Int) -> Int
calculate entry =
    let
        rowChecksum row =
            Maybe.map2 (-)
                (List.maximum row)
                (List.minimum row)
                |> Maybe.withDefault 0
    in
        List.map rowChecksum entry
            |> List.sum


calculate2 : List (List Int) -> Int
calculate2 entry =
    let
        exactDivision a b =
            if rem a b == 0 then
                Just (a // b)
            else if rem b a == 0 then
                Just (b // a)
            else
                Nothing

        findExactDivision : List Int -> Int -> Int -> Int
        findExactDivision row index elem =
            List.map (exactDivision elem) (List.drop (index + 1) row)
                |> List.filterMap identity
                |> List.head
                |> Maybe.withDefault 0

        rowChecksum : List Int -> Int
        rowChecksum row =
            List.indexedMap (findExactDivision row) row
                |> List.sum
    in
        List.map rowChecksum entry
            |> List.sum
