module Grid exposing (CellLine, Grid, Igrid, gatherAndCount3, generateCells, last, makeGrid, mapGrid, nextGrid, randomGrid, rotate, slidGrid, sumCountGrid3, toggleCell)

import Random exposing (..)


type alias CellLine =
    List Bool


type alias Grid =
    List CellLine


makeGrid sizes b =
    let
        ( x, y ) =
            sizes
    in
    List.repeat y <| List.repeat x b


randomGrid : ( Int, Int ) -> Generator Grid
randomGrid gridSizes =
    let
        ( x, y ) =
            gridSizes

        randBool =
            Random.weighted ( 30, True ) [ ( 70, False ) ]
    in
    Random.list y <| Random.list x randBool


mapGrid : (a -> b) -> List (List a) -> List (List b)
mapGrid fn grid =
    List.map (\row -> List.map fn row) grid


getValeAt : Int -> Int -> Grid -> Maybe Bool
getValeAt x y grid =
    let
        nth n ls =
            List.drop n ls |> List.head

        cell =
            case nth y grid of
                Just row ->
                    nth x row

                Nothing ->
                    Nothing
    in
    cell


setValueAt : Int -> Int -> Bool -> Grid -> Grid
setValueAt x y v grid =
    let
        fnCell icol cell =
            if icol == x then
                v

            else
                cell

        fnRow irow row =
            if irow == y then
                List.indexedMap fnCell row

            else
                row
    in
    List.indexedMap fnRow grid


toggleCell : Int -> Int -> Grid -> Grid
toggleCell x y grid =
    case getValeAt x y grid of
        Just v ->
            setValueAt x y (not v) grid

        Nothing ->
            grid


last : List a -> Maybe a
last lis =
    case lis of
        a :: rest ->
            if List.isEmpty rest then
                Just a

            else
                last rest

        _ ->
            Nothing


rotate : Int -> List a -> List a
rotate n lis =
    if n < 0 then
        case lis of
            a :: rest ->
                rest ++ [ a ]

            _ ->
                lis

    else if n > 0 then
        case last lis of
            Just l ->
                let
                    len =
                        List.length lis

                    head =
                        List.take (len - 1) lis
                in
                l :: head

            _ ->
                lis

    else
        lis


slidGrid col row grid =
    let
        g0 =
            rotate row grid

        g1 =
            List.map (\line -> rotate col line) g0
    in
    g1



-- 3つのグリッドの真の合計値を求めた整数値グリッドを返す


gatherAndCount3 : ( Grid, Grid, Grid ) -> List (List Int)
gatherAndCount3 grids =
    let
        ( g0, g1, g2 ) =
            grids

        fnCell x y z =
            List.foldr
                (\b cnt ->
                    if b then
                        cnt + 1

                    else
                        cnt
                )
                0
                [ x, y, z ]

        fnLine x y z =
            List.map3 fnCell x y z

        res =
            List.map3 fnLine g0 g1 g2
    in
    res



-- 3つの整数値グリッドを足して合計の整数値グリッドを返す


type alias Igrid =
    List (List Int)


sumCountGrid3 : ( Igrid, Igrid, Igrid ) -> Igrid
sumCountGrid3 igrids =
    let
        ( g0, g1, g2 ) =
            igrids

        sumLine x y z =
            List.map3 (\a b c -> a + b + c) x y z

        res =
            List.map3 sumLine g0 g1 g2
    in
    res


generateCells : Grid -> Igrid -> Grid
generateCells grid igrid =
    let
        f0 b i =
            if b then
                i == 3 || i == 4

            else
                i == 3

        f1 b i =
            List.map2 f0 b i
    in
    List.map2 f1 grid igrid


nextGrid : Grid -> Grid
nextGrid grid =
    let
        i0 =
            gatherAndCount3 ( slidGrid -1 -1 grid, slidGrid 0 -1 grid, slidGrid 1 -1 grid )

        i1 =
            gatherAndCount3 ( slidGrid -1 0 grid, slidGrid 0 0 grid, slidGrid 1 0 grid )

        i2 =
            gatherAndCount3 ( slidGrid -1 1 grid, slidGrid 0 1 grid, slidGrid 1 1 grid )
    in
    sumCountGrid3 ( i0, i1, i2 ) |> generateCells grid
