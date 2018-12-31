module LifeGame exposing (Model, Msg(..), drawGrid, init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Grid exposing (..)
import Html exposing (Html, button, div, input, option, select, span, text)
import Html.Attributes as Attrs exposing (..)
import Html.Events as Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (..)
import Random exposing (..)


type alias Model =
    { counter : Int
    , cellSizes : ( Int, Int )
    , grid : Grid
    , lastSelectedCell : ( Int, Int )
    }


type Msg
    = NoOp
    | OnStep
    | RandomGrid
    | NewGrid Grid
    | ChanegCellSizeRow String
    | ChanegCellSizeCol String
    | ChanegCellSizes ( Int, Int )
    | ClickOnCell Int Int
    | OnKey String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 ( 10, 10 ) makeGrid ( 0, 0 ), Cmd.none )


drawGrid : Grid -> List (Html Msg)
drawGrid grid =
    -- TODO: リファクタリングするべし!
    let
        colFun rowIndex colIndex cell =
            let
                onClickAttr =
                    onClick <| ClickOnCell colIndex rowIndex

                drawCell color t =
                    span [ style "background-color" color, onClickAttr ] [ text t ]
            in
            if cell then
                drawCell "red" "●"

            else
                drawCell "blue" "\u{3000}"

        rowFun : Grid -> List (Html Msg)
        rowFun rows =
            let
                fn rowIndex row =
                    div [] (List.indexedMap (colFun rowIndex) row)
            in
            List.indexedMap fn rows
    in
    rowFun grid


view model =
    let
        ( rowSize, colSize ) =
            model.cellSizes

        control_frame =
            div []
                [ div []
                    [ text "Control"
                    , div []
                        [ button [ onClick OnStep ] [ text "Step" ]
                        , button [ onClick RandomGrid ] [ text "Random" ]
                        , span [] [ text (String.fromInt model.counter) ]
                        , span
                            [ onClick RandomGrid
                            , style "color" "blue"
                            , style "background-color" "yellow"
                            ]
                            [ text "Clickable" ]
                        , div
                            []
                            [ input [ Attrs.value (String.fromInt rowSize), onInput ChanegCellSizeRow ] []
                            , input [ Attrs.value (String.fromInt colSize), onInput ChanegCellSizeCol ] []
                            ]
                        ]
                    ]
                ]

        status_frame =
            let
                sizeText =
                    "X: " ++ String.fromInt rowSize ++ ", y:  " ++ String.fromInt colSize
            in
            div []
                [ div [] [ text "Status" ]
                , span [] [ text sizeText ]
                ]

        grid_frame =
            div [] [ text "Grid" ]
    in
    div [] ([ control_frame, status_frame, grid_frame ] ++ drawGrid model.grid)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnStep ->
            ( { model
                | counter = model.counter + 1
                , grid = nextGrid model.grid
              }
            , Cmd.none
            )

        RandomGrid ->
            ( model, Random.generate NewGrid (Grid.randomGrid model.cellSizes) )

        NewGrid newGrid ->
            ( { model | grid = newGrid }, Cmd.none )

        ClickOnCell x y ->
            ( { model | grid = Grid.toggleCell x y model.grid }, Cmd.none )

        ChanegCellSizeRow sizeStr ->
            let
                ( origX, origY ) =
                    model.cellSizes

                newX =
                    Maybe.withDefault origX (String.toInt sizeStr)

                newSizes =
                    ( newX, origY )
            in
            update (ChanegCellSizes newSizes) model

        ChanegCellSizeCol sizeStr ->
            let
                ( origX, origY ) =
                    model.cellSizes

                newY =
                    Maybe.withDefault origY (String.toInt sizeStr)

                newSizes =
                    ( origX, newY )
            in
            update (ChanegCellSizes newSizes) model

        ChanegCellSizes newSizes ->
            ( { model | cellSizes = newSizes }, Cmd.none )

        OnKey key ->
            case key of
                " " ->
                    update OnStep model

                "Enter" ->
                    update OnStep model

                "r" ->
                    update RandomGrid model

                _ ->
                    ( model, Cmd.none )


keyDecoder =
    Decode.field "key" Decode.string


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map OnKey keyDecoder)
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
