module ChecksumTest exposing (..)

import Checksum
import Test exposing (Test, describe, test)
import Expect


checksum1 : Test
checksum1 =
    describe "checksum 1"
        [ test "sum of the difference between the largest and the smallest element for each column" <|
            \() ->
                Checksum.calculate
                    [ [ 5, 1, 9, 5 ]
                    , [ 9, 2, 4 ]
                    ]
                    |> Expect.equal 15
        , test "returns 0 for empty lists" <|
            \() ->
                Checksum.calculate []
                    |> Expect.equal 0
        ]


checksum2 : Test
checksum2 =
    describe "checksum 2"
        [ test "finds a pair of numbers on each row that divides exactly and sum the results of the division" <|
            \() ->
                Checksum.calculate2
                    [ [ 5, 9, 2, 8 ]
                    , [ 9, 4, 7, 3 ]
                    , [ 3, 8, 6, 5 ]
                    ]
                    |> Expect.equal 9
        ]
