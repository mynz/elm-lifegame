module LifeGame exposing (Model, Msg(..), drawGrid, init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Grid exposing (..)
import Html exposing (Html, button, div, input, label, option, select, span, text)
import Html.Attributes as Attrs exposing (..)
import Html.Events as Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (..)
import Random exposing (..)


type alias Model =
    { counter : Int
    , cellSizes : ( Int, Int )
    , grid : Grid
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
    let
        sizes =
            ( 10, 10 )
    in
    ( Model 0 sizes (makeGrid sizes False), Cmd.none )


drawGrid : Grid -> List (Html Msg)
drawGrid grid =
    let
        fnCell y x e =
            let
                styleCommon =
                    [ style "padding" "0px"
                    , style "width" "30px"
                    , style "height" "30px"
                    , style "text-align" "center"
                    , style "vertical-align" "middle"
                    , style "border" "solid"
                    , style "border-width" "thin"
                    , style "background-color" "red"
                    , onClick <| ClickOnCell x y
                    ]

                styleTrue =
                    [ style "background" "red"
                    ]

                styleFalse =
                    [ style "background" "white"
                    ]
            in
            if e then
                Html.td (styleCommon ++ styleTrue) [ text "o" ]

            else
                Html.td (styleCommon ++ styleFalse) [ text "x" ]

        fnRow y e =
            Html.tr [] <| List.indexedMap (fnCell y) e

        rows =
            List.indexedMap fnRow grid

        table =
            Html.table
                [ style "border-collapse" "collapse"
                , style "table-layout" "fixed"
                ]
                rows
    in
    [ table ]


view model =
    let
        ( rowSize, colSize ) =
            model.cellSizes

        controlDiv =
            let
                labelAttrs =
                    [ style "margin" "10px" ]

                inputAttrs val msg =
                    [ Attrs.size 5
                    , Attrs.value <| String.fromInt val
                    , onInput msg
                    ]

                sizeInputDiv =
                    div
                        [ style "text-indent" "1em" ]
                        [ label labelAttrs
                            [ text "width: "
                            , input
                                (inputAttrs rowSize ChanegCellSizeRow)
                                []
                            ]
                        , label labelAttrs
                            [ text "height: "
                            , input (inputAttrs colSize ChanegCellSizeCol) []
                            ]
                        ]
            in
            div []
                [ div []
                    [ text "Control"
                    , sizeInputDiv
                    ]
                ]

        statusDiv =
            let
                textSteps =
                    "steps: " ++ String.fromInt model.counter
            in
            div []
                [ text "Status"
                , div [ style "text-indent" "1em" ] [ text textSteps ]
                ]
    in
    div []
        ([ controlDiv
         , statusDiv

         --, grid_frame
         ]
            ++ drawGrid model.grid
        )


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
