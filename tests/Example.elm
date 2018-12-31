module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Check how tests work"
        [ test "xxx" <|
            \_ -> Expect.equal 4 (2 + 2)
        , test "null" <|
            \_ -> Expect.equal True True
        , test "one" <|
            \_ -> Expect.equal makeGrid makeGrid
        , test "last" <|
            \_ -> Expect.equal (last [ 1, 2, 3 ]) (Just 3)
        , test "gather" <|
            \_ ->
                let
                    grid =
                        [ [ True, False ]
                        ]

                    res =
                        gatherAndCount3 ( grid, grid, grid )

                    expected =
                        [ [ 3, 0 ] ]
                in
                Expect.equal res expected
        , test "count cells" <|
            \_ ->
                let
                    igrid =
                        [ [ 0, 1, 2 ]
                        ]

                    res =
                        sumCountGrid3 ( igrid, igrid, igrid )

                    expected =
                        [ [ 0, 3, 6 ] ]
                in
                Expect.equal res expected
        , test "generate cells" <|
            \_ ->
                let
                    grid =
                        [ [ True, True, True, True, True ]
                        , [ False, False, False, False, False ]
                        ]

                    igrid =
                        [ [ 1, 2, 3, 4, 5 ]
                        , [ 1, 2, 3, 4, 5 ]
                        ]

                    res =
                        generateCells grid igrid

                    expected =
                        [ [ False, False, True, True, False ]
                        , [ False, False, True, False, False ]
                        ]
                in
                Expect.equal expected res
        , test "nextGrid 0" <|
            \_ ->
                let
                    grid =
                        [ [ False, False, False, False ]
                        , [ False, True, True, False ]
                        , [ False, True, True, False ]
                        , [ False, False, False, False ]
                        ]

                    expected =
                        [ [ False, False, False, False ]
                        , [ False, True, True, False ]
                        , [ False, True, True, False ]
                        , [ False, False, False, False ]
                        ]

                    res =
                        nextGrid grid
                in
                Expect.equal expected res
        , test "nextGrid 1" <|
            \_ ->
                let
                    grid =
                        [ [ False, False, False, False, False ]
                        , [ False, False, False, False, False ]
                        , [ False, False, True, False, False ]
                        , [ False, False, False, False, False ]
                        ]

                    expected =
                        [ [ False, False, False, False, False ]
                        , [ False, False, False, False, False ]
                        , [ False, False, False, False, False ]
                        , [ False, False, False, False, False ]
                        ]

                    res =
                        nextGrid grid
                in
                Expect.equal expected res
        , test "toggleCell" <|
            \_ ->
                let
                    grid =
                        [ [ False, True ]
                        , [ True, False ]
                        ]

                    res =
                        toggleCell 1 0 grid

                    expected =
                        [ [ False, False ]
                        , [ True, False ]
                        ]
                in
                Expect.equal expected res
        ]
