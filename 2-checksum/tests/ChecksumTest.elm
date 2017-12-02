module ChecksumTest exposing (..)

import Checksum
import Test exposing (Test, describe, test)
import Expect


suite : Test
suite =
    describe "checksum"
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
