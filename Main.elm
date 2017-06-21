module Main exposing (..)

import Html exposing (Html, div)
import Html.App
import Graphics.Render exposing (..)
import Color exposing (rgb)
import Random exposing (Generator, list, initialSeed)
import Random.Extra exposing (constant, frequency)
import Random.Color as RandomC


canvasWidth : Float
canvasWidth =
    1680


canvasHeight : Float
canvasHeight =
    880


type alias Model =
    Maybe Int


type alias Msg =
    Int


type Direction
    = North
    | East
    | South
    | West
    | NorthEast
    | SouthEast
    | SouthWest
    | NorthWest


type Move
    = Straight
    | DiagonalLeft
    | DiagonalRight
    | TurnLeft
    | TurnRight


randomMove : Generator Move
randomMove =
    frequency
        [ ( 2, constant Straight )
        , ( 1, constant TurnLeft )
        , ( 1, constant TurnRight )
        , ( 1, constant DiagonalLeft )
        , ( 1, constant DiagonalRight )
        ]


createPath : List Move -> List Point
createPath moves =
    let
        distance =
            25

        init =
            ( ( 0, 0 ), NorthEast, [] )

        makeMove move ( ( x, y ), dir, result ) =
            let
                newDir =
                    case ( dir, move ) of
                        ( NorthEast, m ) ->
                            case m of
                                Straight ->
                                    NorthEast

                                DiagonalLeft ->
                                    NorthWest

                                DiagonalRight ->
                                    SouthEast

                                TurnLeft ->
                                    North

                                TurnRight ->
                                    East

                        ( SouthEast, m ) ->
                            case m of
                                Straight ->
                                    SouthEast

                                DiagonalLeft ->
                                    NorthEast

                                DiagonalRight ->
                                    SouthWest

                                TurnLeft ->
                                    East

                                TurnRight ->
                                    South

                        ( SouthWest, m ) ->
                            case m of
                                Straight ->
                                    SouthWest

                                DiagonalLeft ->
                                    SouthEast

                                DiagonalRight ->
                                    NorthWest

                                TurnLeft ->
                                    South

                                TurnRight ->
                                    West

                        ( NorthWest, m ) ->
                            case m of
                                Straight ->
                                    NorthWest

                                DiagonalLeft ->
                                    SouthWest

                                DiagonalRight ->
                                    NorthEast

                                TurnLeft ->
                                    West

                                TurnRight ->
                                    North

                        ( North, m ) ->
                            case m of
                                Straight ->
                                    North

                                DiagonalLeft ->
                                    NorthWest

                                DiagonalRight ->
                                    NorthEast

                                TurnLeft ->
                                    West

                                TurnRight ->
                                    East

                        ( East, m ) ->
                            case m of
                                Straight ->
                                    East

                                DiagonalLeft ->
                                    NorthEast

                                DiagonalRight ->
                                    SouthEast

                                TurnLeft ->
                                    North

                                TurnRight ->
                                    South

                        ( South, m ) ->
                            case m of
                                Straight ->
                                    South

                                DiagonalLeft ->
                                    SouthEast

                                DiagonalRight ->
                                    SouthWest

                                TurnLeft ->
                                    East

                                TurnRight ->
                                    West

                        ( West, m ) ->
                            case m of
                                Straight ->
                                    West

                                DiagonalLeft ->
                                    SouthWest

                                DiagonalRight ->
                                    NorthWest

                                TurnLeft ->
                                    South

                                TurnRight ->
                                    North

                newPos =
                    case newDir of
                        North ->
                            ( x, y + distance )

                        East ->
                            ( x + distance, y )

                        South ->
                            ( x, y - distance )

                        West ->
                            ( x - distance, y )

                        NorthEast ->
                            ( x + distance, y + distance )

                        SouthEast ->
                            ( x + distance, y - distance )

                        SouthWest ->
                            ( x - distance, y - distance )

                        NorthWest ->
                            ( x - distance, y + distance )
            in
                ( newPos, newDir, newPos :: result )
    in
        List.foldl makeMove init moves
            |> (\( _, _, result ) -> result)


view : Maybe Int -> Html Msg
view seedBase =
    case seedBase of
        Nothing ->
            rectangle canvasWidth canvasHeight
                |> solidFill (rgb 15 15 15)
                |> svg canvasWidth canvasHeight

        Just seed ->
            group
                [ rectangle canvasWidth canvasHeight
                    |> solidFill (rgb 15 15 15)
                , [0..100]
                    |> List.map (\x -> (initialSeed (x + seed)))
                    |> List.map (Random.step (list 100 randomMove) >> fst)
                    |> List.map
                        (createPath
                            >> polyline
                            >> solid 2
                                ((Random.step RandomC.rgb) (initialSeed seed) |> fst)
                            >> opacity 0.2
                        )
                    |> group
                ]
                |> svg canvasWidth canvasHeight


update : Msg -> Model -> Maybe Msg
update msg model =
    Just msg


main : Program Never
main =
    Html.App.program
        { init = ( Nothing, Random.generate identity (Random.int -1000000 10000000) )
        , subscriptions = \_ -> Sub.none
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        }
