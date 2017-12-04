module SpiralMemory exposing (..)

import Array


stopWhenHelper : (Int -> Bool) -> List Int -> Int -> Int
stopWhenHelper predicate list pos =
    case list of
        [] ->
            pos

        x :: xs ->
            if (predicate x) then
                pos
            else
                stopWhenHelper predicate xs (pos + 1)


stopWhen : (Int -> Bool) -> List Int -> Int
stopWhen predicate list =
    stopWhenHelper predicate list 0


rangeHelper : List Int -> Int -> Int -> List Int
rangeHelper entry current step =
    case current of
        0 ->
            entry

        n ->
            let
                next =
                    List.head entry |> Maybe.withDefault 0 |> (+) step
            in
                rangeHelper (next :: entry) (current - 1) step


rangeWithStep : Int -> Int -> List Int
rangeWithStep max stepInc =
    rangeHelper [ 1 ] (max - 1) stepInc
        |> List.reverse


whichLayer : Int -> Int
whichLayer entry =
    stopWhen (\n -> entry <= n ^ 2)
        (rangeWithStep (toFloat entry |> sqrt |> round) 2)


numberOfSteps : Int -> Int
numberOfSteps entry =
    let
        layer =
            whichLayer entry

        sideSize =
            1 + (2 * layer)

        lastInLayer =
            sideSize ^ 2

        middleDistance =
            sideSize // 2

        middles =
            [ lastInLayer - (middleDistance * 7)
            , lastInLayer - (middleDistance * 5)
            , lastInLayer - (middleDistance * 3)
            , lastInLayer - middleDistance
            ]
                |> Array.fromList

        side =
            List.range 0 3
                |> List.filter (\n -> entry > lastInLayer - ((n + 1) * (sideSize - 1)))
                |> List.head
                |> Maybe.withDefault 3
                |> (\n -> 3 - n)

        distanceFromTheMiddle =
            Array.get side middles
                |> Maybe.withDefault 0
                |> \x -> abs (x - entry)
    in
        distanceFromTheMiddle + layer
