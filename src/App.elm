module App exposing (..)

import Utils exposing (..)
import Styles exposing (..)
import Html exposing (Html, text, div, input, button, span, h3, h5, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import List exposing (length, filter, filterMap, indexedMap, drop, take, range, head, tail, foldl, foldr, member, sortBy)
import String


-- Scalable Tic Tac Toe!
-- MODEL


getDefaultScale =
    identity 3


type alias Model =
    { padding : Int
    , size : Int
    , scale : Int
    , hasNoMoves : Bool
    , currentMove : Int
    , boardStates : List (List ( Int, String ))
    , currentBoard : List ( Int, String )
    , winner : Maybe String
    }


reset x =
    let
        n =
            case (x < 3) of
                True ->
                    3

                False ->
                    x
    in
        { padding = 5
        , size = 60
        , scale = n
        , hasNoMoves = False
        , currentMove = -1
        , boardStates = []
        , currentBoard = List.map (\i -> ( i, "" )) (range 1 (n * n))
        , winner = Nothing
        }



-- INIT


init =
    ( reset getDefaultScale, Cmd.none )



-- UPDATE


type Msg
    = UserMove Int
    | AIMove
    | UserTryAgain
    | OnScaleChange String


update msg model =
    let
        -- Grab current board
        { scale, currentBoard, winner } =
            model

        -- Scan the board
        scanBoard x board =
            let
                m =
                    matrixify x board
            in
                m ++ (transpose m) ++ (diagRight m) ++ (diagLeft m)

        -- Find next move, based on custom strategy
        findNextMove strategy =
            let
                -- Common strategy (find all rows taken)
                common row =
                    if (length (filter (\( _, y ) -> y /= "") row)) == scale then
                        Nothing
                    else
                        Just row

                -- Find best move candidate
                findCandidate rows =
                    let
                        candidates =
                            filterMap common rows
                                |> strategy
                    in
                        case (length candidates) of
                            0 ->
                                Nothing

                            _ ->
                                head candidates

                all =
                    scanBoard scale currentBoard

                candidate =
                    findCandidate all
            in
                case candidate of
                    Just row ->
                        let
                            tuples =
                                filter (\( _, t ) -> t == "") row
                        in
                            case (head tuples) of
                                Just ( i, _ ) ->
                                    i

                                Nothing ->
                                    -1

                    Nothing ->
                        -1

        -- Update board
        updateBoard char target =
            let
                f _ ( i, x ) =
                    if i == target then
                        if x == "" then
                            ( i, char )
                        else
                            ( i, x )
                    else
                        ( i, x )

                state =
                    indexedMap f currentBoard

                hasNoMoves =
                    (length (filter (\( _, x ) -> x == "") state)) == 0
            in
                ( hasNoMoves, state )
    in
        case winner of
            Just _ ->
                case msg of
                    UserTryAgain ->
                        ( reset scale, Cmd.none )

                    OnScaleChange n ->
                        ( reset (Result.withDefault getDefaultScale (String.toInt n)), Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            _ ->
                case msg of
                    UserMove index ->
                        let
                            ( hasNoMoves, board ) =
                                updateBoard "o" index
                        in
                            ( { model | currentBoard = board, hasNoMoves = hasNoMoves }, send AIMove )

                    AIMove ->
                        let
                            findNumOfChar char r =
                                let
                                    numOfChar char ( _, y ) =
                                        y == char
                                in
                                    length (filter (numOfChar char) r)

                            isIndexEmpty index =
                                member ( index, "" ) currentBoard

                            findCharAt index =
                                filter (\( i, x ) -> i == index) currentBoard
                                    |> List.map (\( i, x ) -> x)
                                    |> List.head
                                    |> Maybe.withDefault ""

                            rowHasChar char row =
                                (findNumOfChar char row) > 0

                            -- Find AI winning move
                            aIWinningMoveCondition rows =
                                filter (\row -> (findNumOfChar "x" row) == (scale - 1)) rows

                            -- If yes, win it!
                            -- If none, find Player's winning move
                            playerWinningMoveCondition rows =
                                filter (\row -> (findNumOfChar "o" row) == (scale - 1)) rows

                            -- If yes, block it!
                            -- If none, decide AI's next move
                            aiNextMoveStrategy rows =
                                let
                                    -- grab center first
                                    center =
                                        ceiling ((toFloat (scale * scale)) / 2)

                                    -- sort by score
                                    sorted =
                                        let
                                            sorter row =
                                                let
                                                    len =
                                                        findNumOfChar "" row

                                                    opponentMoves =
                                                        findNumOfChar "o" row
                                                in
                                                    case opponentMoves of
                                                        0 ->
                                                            len

                                                        _ ->
                                                            len + scale
                                        in
                                            sortBy sorter (List.reverse rows)

                                    default =
                                        let
                                            rest =
                                                case sorted of
                                                    [] ->
                                                        []

                                                    x :: xs ->
                                                        [ x ]
                                        in
                                            case (findCharAt center) of
                                                "o" ->
                                                    -- Grab corner
                                                    if (member ( scale, "" ) currentBoard) then
                                                        [ [ ( scale, "" ) ] ]
                                                    else
                                                        rest

                                                _ ->
                                                    rest
                                in
                                    case (isIndexEmpty center) of
                                        True ->
                                            [ [ ( center, "" ) ] ]

                                        False ->
                                            case (findCharAt center) of
                                                "o" ->
                                                    case (isIndexEmpty scale) of
                                                        True ->
                                                            [ [ ( scale, "" ) ] ]

                                                        False ->
                                                            default

                                                "x" ->
                                                    case (isIndexEmpty (scale - 1)) of
                                                        True ->
                                                            [ [ ( (scale - 1), "" ) ] ]

                                                        False ->
                                                            default

                                                _ ->
                                                    default
                        in
                            case (findNextMove aIWinningMoveCondition) of
                                -1 ->
                                    case (findNextMove playerWinningMoveCondition) of
                                        -1 ->
                                            case (findNextMove aiNextMoveStrategy) of
                                                -1 ->
                                                    ( model, Cmd.none )

                                                g ->
                                                    let
                                                        ( hasNoMoves, board ) =
                                                            updateBoard "x" g
                                                    in
                                                        ( { model | currentBoard = board, hasNoMoves = hasNoMoves }, Cmd.none )

                                        n ->
                                            let
                                                ( hasNoMoves, board ) =
                                                    updateBoard "x" n
                                            in
                                                ( { model | currentBoard = board, hasNoMoves = hasNoMoves }, Cmd.none )

                                m ->
                                    let
                                        ( hasNoMoves, board ) =
                                            updateBoard "x" m
                                    in
                                        ( { model | currentBoard = board, winner = Just "x", hasNoMoves = hasNoMoves }, Cmd.none )

                    UserTryAgain ->
                        ( reset scale, Cmd.none )

                    OnScaleChange n ->
                        ( reset (Result.withDefault getDefaultScale (String.toInt n)), Cmd.none )



-- VIEW


view model =
    div [ styleRoot ]
        [ viewPanel model
        , viewBoard model
        ]


viewPanel { scale } =
    let
        fontSize =
            "18px"

        styleLabel =
            style
                [ ( "display", "inline-block" )
                , ( "font-size", fontSize )
                , ( "margin-right", "10px" )
                ]

        styleInput =
            style
                [ ( "width", "50px" )
                , ( "font-size", fontSize )
                ]
    in
        div [ stylePanel ]
            [ h3 [] [ text "Scalable Tic-Tac-Toe!" ]
            , h5 [] [ a [ href "https://github.com/thomasloh/elm-scalable-tic-tac-toe", target "_blank" ] [ text "[Source]" ] ]
            , div []
                [ span [ styleLabel ] [ text "Scale:" ]
                , input [ type_ "number", value (toString scale), onInput OnScaleChange, Html.Attributes.max "20", styleInput ] []
                ]
            ]


viewBoard model =
    let
        { padding, size, scale, winner, currentBoard, hasNoMoves } =
            model

        result =
            let
                try t =
                    div [ style [ ( "text-align", "left" ) ] ]
                        [ button [ onClick UserTryAgain ] [ text t ]
                        ]
            in
                case winner of
                    Just "x" ->
                        try "You lost, try again."

                    _ ->
                        if hasNoMoves then
                            try "It's a tie, try again"
                        else
                            div [] []
    in
        div [ style [ ( "flex", "1" ), ( "padding-top", "90px" ) ] ]
            [ div [ (styleBoard model) ]
                (List.map (viewBoardRowBox model) currentBoard)
            , result
            ]


viewBoardRowBox model ( i, board ) =
    div [ (styleRowBox model), onClick (UserMove i) ]
        [ text board
        ]
