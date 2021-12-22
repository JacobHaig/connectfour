module Main exposing (Board, Model, Piece(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Element
import Element.Border
import Element.Events
import Element.Font
import Html exposing (Html)
import List.Extra
import Svg
import Svg.Attributes


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
    , turn : Bool
    , board : Board
    , hovering : Maybe Int
    }


initBoard : Int -> Int -> Board
initBoard width height =
    Array.repeat width (Array.repeat height EMPTY)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boardWidth = 9
      , boardHeight = 6
      , turn = True
      , hovering = Nothing
      , board = initBoard 9 6
      }
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentPiece =
            if model.turn then
                RED

            else
                BLACK
    in
    case msg of
        Hover columnIndex ->
            ( { model | hovering = Just columnIndex }
            , Cmd.none
            )

        Reset ->
            ( { model
                | board = initBoard model.boardWidth model.boardHeight
                , turn = True
              }
            , Cmd.none
            )

        Click column ->
            ( { model
                | board = dropPiece model.board column currentPiece
                , turn = not model.turn
              }
            , Cmd.none
            )


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

                EMPTY ->
                    viewCircle "lightgrey"

                OUTOFBOUNDS ->
                    viewCircle "grey"
    in
    Element.el [] pieceSvg


viewBoard : Model -> Element.Element Msg
viewBoard model =
    let
        elements =
            model.board
                |> Array.toList
                |> List.indexedMap
                    (\indexColumn column ->
                        column
                            |> Array.toList
                            |> List.map (\ele -> viewApplyCircle ele)
                            |> Element.column [ Element.Events.onClick (Click indexColumn) ]
                    )
                |> Element.row []
    in
    elements


viewCurrentTurn : Model -> Element.Element Msg
viewCurrentTurn model =
    let
        playerText =
            if model.turn then
                "Red"

            else
                "Black"
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
            [ viewCurrentTurn model, viewBoard model, viewResetButton ]
