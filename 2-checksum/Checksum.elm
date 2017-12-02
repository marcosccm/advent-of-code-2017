module Checksum exposing (calculate)


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
