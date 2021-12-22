module Main exposing (Board, Model, Piece(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (Element)
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Html exposing (Html, button, div, text)
import Svg
import Svg.Attributes
import Task


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Board =
    Array (Array Piece)


type Piece
    = RED
    | BLACK
    | EMPTY
    | OUTOFBOUNDS


type Msg
    = Click Int Int
    | Reset


type alias Model =
    { boardWidth : Int
    , boardHeight : Int
    , turn : Bool
    , board : Board
    }


initBoard : Int -> Int -> Board
initBoard width height =
    Array.repeat height <| Array.repeat width EMPTY


init : () -> ( Model, Cmd Msg )
init _ =
    ( { boardWidth = 9
      , boardHeight = 6
      , turn = True
      , board = initBoard 9 6
      }
    , Cmd.none
    )


updateBoard : Model -> Board -> Int -> Int -> Board
updateBoard model board x y =
    let
        row =
            Array.get y board
                |> Maybe.withDefault Array.empty

        oldPiece =
            Array.get x row
                |> Maybe.withDefault OUTOFBOUNDS

        newPiece =
            if oldPiece == EMPTY then
                if model.turn then
                    RED

                else
                    BLACK

            else
                oldPiece
    in
    Array.set y (Array.set x newPiece row) board


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model
                | board = initBoard model.boardWidth model.boardHeight
                , turn = True
              }
            , Cmd.none
            )

        Click x y ->
            let
                row =
                    Array.get y model.board
                        |> Maybe.withDefault Array.empty

                oldPiece =
                    Array.get x row
                        |> Maybe.withDefault OUTOFBOUNDS

                rowBelow =
                    Array.get (y + 1) model.board
                        |> Maybe.withDefault Array.empty

                oldPieceBelow =
                    Array.get x rowBelow
                        |> Maybe.withDefault OUTOFBOUNDS
            in
            -- If the piece below is empty, then we move our placement down.
            if oldPieceBelow == EMPTY then
                update (Click x (y + 1)) model

            else if oldPiece == EMPTY then
                ( { model
                    | turn = not model.turn
                    , board = updateBoard model model.board x y
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewCircle : String -> Element.Element Msg
viewCircle color =
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width "60"
            , Svg.Attributes.height "60"
            , Svg.Attributes.viewBox <| "0 0 60 60"
            ]
            [ Svg.circle
                [ Svg.Attributes.cx "30"
                , Svg.Attributes.cy "30"
                , Svg.Attributes.r "25"
                , Svg.Attributes.fill color
                , Svg.Attributes.stroke "black"
                , Svg.Attributes.strokeWidth "1"
                ]
                []
            ]


viewSlot : Model -> Int -> Int -> Element.Element Msg
viewSlot model x y =
    let
        rows =
            Maybe.withDefault Array.empty (Array.get y model.board)

        piece =
            Maybe.withDefault EMPTY (Array.get x rows)

        circle =
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
    Element.el
        [ Element.Events.onClick (Click x y) ]
        circle


viewCurrentTurn : Model -> Element.Element Msg
viewCurrentTurn model =
    let
        playerText =
            case model.turn of
                True ->
                    "Red"

                False ->
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
    let
        boardList =
            List.map Array.toList (Array.toList model.board)

        boardEl =
            List.indexedMap (\y row -> List.indexedMap (\x ele -> viewSlot model x y) row) boardList

        theBoard =
            Element.column [] (List.indexedMap (\y row -> Element.row [] row) boardEl)
    in
    Element.layout [] <|
        Element.column [ Element.centerX, Element.centerY ]
            [ viewCurrentTurn model, theBoard, viewResetButton ]
