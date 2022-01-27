module Main exposing (Board, Model, Piece(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Html exposing (Html)
import List
import List.Extra
import Platform.Cmd exposing (Cmd)
import Svg
import Svg.Attributes
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Column =
    Array Piece


type alias Board =
    Array Column


type Piece
    = RED
    | BLACK
    | EMPTY
    | OUTOFBOUNDS


type Msg
    = Click Int
    | Hover Int
    | Reset


type alias Model =
    { boardWidth : Int
    , boardHeight : Int
    , turn : Piece
    , board : Board
    , hovering : Maybe Int
    , winner : Maybe Piece
    }


initBoard : Int -> Int -> Board
initBoard width height =
    Array.repeat width (Array.repeat height EMPTY)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boardWidth = 9
      , boardHeight = 6
      , turn = RED
      , hovering = Nothing
      , board = initBoard 9 6
      , winner = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hover columnIndex ->
            ( { model | hovering = Just columnIndex }
            , Cmd.none
            )

        Reset ->
            ( { model
                | board = initBoard model.boardWidth model.boardHeight
                , turn = RED
                , hovering = Nothing
                , winner = Nothing
              }
            , Cmd.none
            )

        Click column ->
            let
                newmodel =
                    if model.winner == Nothing then
                        { model
                            | board = dropPiece model.board column model.turn
                        }

                    else
                        model
            in
            ( if model.winner == Nothing then
                { model
                    | board = newmodel.board
                    , winner = checkForWinner newmodel
                    , turn =
                        case model.turn of
                            RED ->
                                BLACK

                            BLACK ->
                                RED

                            _ ->
                                Debug.todo "branch not implemented"
                }

              else
                model
            , Cmd.none
            )


dropPiece : Board -> Int -> Piece -> Board
dropPiece board columnIndex piece =
    let
        column =
            board
                |> Array.get columnIndex
                |> Maybe.withDefault Array.empty

        height : ( Int, Piece )
        height =
            column
                |> Array.toIndexedList
                |> List.reverse
                |> List.Extra.find (\( _, p ) -> p == EMPTY)
                |> Maybe.withDefault ( 0, OUTOFBOUNDS )

        newColumn =
            Array.set (Tuple.first height) piece column

        newBoard =
            -- Only change the board if the is a valid drop
            if Tuple.second height /= OUTOFBOUNDS then
                Array.set columnIndex newColumn board

            else
                board
    in
    newBoard


checkForWinner : Model -> Maybe Piece
checkForWinner model =
    let
        board_list =
            model.board
                |> Array.map
                    (\col -> Array.toList col)
                |> Array.toList

        rx x y =
            -- Horizontal
            [ xyHelper x y 0 0
            , xyHelper x y 0 1
            , xyHelper x y 0 2
            , xyHelper x y 0 3
            ]

        ry x y =
            -- Vertical
            [ xyHelper x y 0 0
            , xyHelper x y 1 0
            , xyHelper x y 2 0
            , xyHelper x y 3 0
            ]

        d1 x y =
            -- Diagonal 1
            [ xyHelper x y 0 0
            , xyHelper x y 1 1
            , xyHelper x y 2 2
            , xyHelper x y 3 3
            ]

        d2 x y =
            -- Diagonal 2
            [ xyHelper x y 3 0
            , xyHelper x y 2 1
            , xyHelper x y 1 2
            , xyHelper x y 0 3
            ]

        xyHelper x y dx dy =
            Maybe.withDefault OUTOFBOUNDS <| List.Extra.getAt (y + dy) <| Maybe.withDefault [ OUTOFBOUNDS ] <| List.Extra.getAt (x + dx) board_list

        checkforWin : Int -> Int -> Bool
        checkforWin x y =
            -- Compare all winning conditions against the coordinates
            [ rx, ry, d1, d2 ]
                |> List.map
                    (\func ->
                        case func x y of
                            RED :: RED :: RED :: RED :: [] ->
                                True

                            BLACK :: BLACK :: BLACK :: BLACK :: [] ->
                                True

                            _ ->
                                False
                    )
                |> List.any (\t -> t)

        results =
            List.range 0 model.boardWidth
                |> List.map
                    (\x ->
                        List.range 0 model.boardHeight
                            |> List.map (\y -> checkforWin x y)
                            |> List.any (\t -> t)
                    )
                |> List.any (\t -> t)
    in
    if results then
        Maybe.Just model.turn

    else
        Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


viewCircle : String -> Element.Element Msg
viewCircle color =
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width "60"
            , Svg.Attributes.height "60"
            ]
            [ Svg.circle
                [ Svg.Attributes.cx "30"
                , Svg.Attributes.cy "30"
                , Svg.Attributes.r "25"
                , Svg.Attributes.fill color
                ]
                []
            ]


viewApplyCircle : Piece -> Element.Element Msg
viewApplyCircle piece =
    let
        pieceSvg =
            case piece of
                RED ->
                    viewCircle "red"

                BLACK ->
                    viewCircle "black"

                _ ->
                    viewCircle "lightgrey"
    in
    Element.el [] pieceSvg


viewBoard : Model -> Element.Element Msg
viewBoard model =
    let
        isHovering indexColumn =
            Maybe.withDefault -1 model.hovering == indexColumn

        columnAttr indexColumn =
            [ Element.Events.onClick (Click indexColumn)
            , Element.Events.onMouseEnter (Hover indexColumn)
            , Element.Border.rounded 50
            , Element.padding 1

            -- If the column is the current hovering column, highlight it
            , if isHovering indexColumn then
                case model.turn of
                    RED ->
                        Element.Background.color (Element.rgba255 255 0 0 0.55)

                    BLACK ->
                        Element.Background.color (Element.rgba255 0 0 0 0.55)

                    _ ->
                        Element.Background.color (Element.rgba255 255 0 0 0.55)

              else
                Element.Background.color (Element.rgba255 255 255 255 0.0)
            ]
    in
    model.board
        |> Array.toList
        |> List.indexedMap
            (\indexColumn column ->
                column
                    |> Array.toList
                    |> List.map (\ele -> viewApplyCircle ele)
                    |> Element.column
                        (columnAttr indexColumn)
            )
        |> Element.row []


viewCurrentTurn : Model -> Element.Element Msg
viewCurrentTurn model =
    let
        playerText =
            case model.winner of
                Nothing ->
                    case model.turn of
                        RED ->
                            "Red's turn"

                        BLACK ->
                            "Black's turn"

                        _ ->
                            Debug.todo "Not implemented"

                Just winner ->
                    case winner of
                        RED ->
                            "Red Wins!"

                        BLACK ->
                            "Black Wins!"

                        _ ->
                            Debug.todo "Not implemented"
    in
    Element.el
        [ Element.width Element.fill
        , Element.paddingXY 5 5
        , Element.centerX
        , Element.Font.size 30
        ]
        (Element.el [ Element.centerX ] <| Element.text playerText)


viewResetButton =
    Element.el
        [ Element.Events.onClick Reset
        , Element.Border.width 1
        , Element.paddingXY 5 5
        ]
        (Element.text "Reset")


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column [ Element.centerX, Element.centerY ]
            [ viewCurrentTurn model
            , viewBoard model
            , viewResetButton
            ]
