module SpiralMemoryTest exposing (..)

import SpiralMemory
import Expect exposing (Expectation)
import Test exposing (..)


whichLayer : Test
whichLayer =
    describe "whichLayer returns the layer for a given number"
        [ test "1 is on layer 0" <|
            \() ->
                SpiralMemory.whichLayer 1
                    |> Expect.equal 0
        , test "2 is on layer 1" <|
            \() ->
                SpiralMemory.whichLayer 2
                    |> Expect.equal 1
        , test "9 is on layer 1" <|
            \() ->
                SpiralMemory.whichLayer 9
                    |> Expect.equal 1
        , test "10 is on layer 2" <|
            \() ->
                SpiralMemory.whichLayer 10
                    |> Expect.equal 2
        ]


numberOfSteps : Test
numberOfSteps =
    describe "returns the number of steps to retrieve a given number"
        [ test "1 takes 0 steps" <|
            \() ->
                SpiralMemory.numberOfSteps 1
                    |> Expect.equal 0
        , test "12 takes 3 steps" <|
            \() ->
                SpiralMemory.numberOfSteps 12
                    |> Expect.equal 3
        , test "1024 takes 31 steps" <|
            \() ->
                SpiralMemory.numberOfSteps 1024
                    |> Expect.equal 31
        ]
