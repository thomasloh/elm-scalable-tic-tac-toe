module Utils exposing (..)

import List exposing (length, filterMap, drop, take, head, tail, foldl, foldr)
import Task


send msg =
    Task.succeed msg
        |> Task.perform identity



-- Matrix functions


matrixify size list =
    case list of
        [] ->
            []

        _ ->
            (take size list) :: (matrixify size (drop size list))


transpose matrix =
    case matrix of
        [] ->
            []

        [] :: xss ->
            transpose xss

        (x :: xs) :: xss ->
            let
                heads =
                    filterMap head xss

                tails =
                    filterMap tail xss
            in
                (x :: heads) :: (transpose (xs :: tails))


diagRight matrix =
    let
        f row c =
            c ++ [ (Maybe.withDefault ( -1, "" ) (head (drop (length c) row))) ]
    in
        [ foldl f [] matrix ]


diagLeft matrix =
    let
        f row c =
            [ (Maybe.withDefault ( -1, "" ) (head (drop (length c) row))) ] ++ c
    in
        [ foldr f [] matrix ]
