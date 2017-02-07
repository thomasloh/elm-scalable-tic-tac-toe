module Main exposing (..)

import App exposing (..)
import Html exposing (Html)


main : Program Never Model Msg
main =
    Html.program { view = view, init = init, update = update, subscriptions = \model -> Sub.none }
