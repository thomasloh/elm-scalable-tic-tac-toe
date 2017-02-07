module Styles exposing (..)

import Html.Attributes exposing (style)


-- STYLES


styleRoot =
    style
        [ ( "display", "flex" )
        , ( "align-items", "center" )
        , ( "justify-content", "center" )
        , ( "flex-direction", "column" )
        , ( "padding", "30px 30px" )
        ]


stylePanel =
    style
        [ ( "flex", "1" )
        , ( "align-self", "flex-start" )
        , ( "text-align", "left" )
        ]


styleBoard { padding, size, scale } =
    let
        maxWidth =
            (toString ((size + padding * 2) * scale)) ++ "px"
    in
        style
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            , ( "flex-direction", "row" )
            , ( "align-items", "center" )
            , ( "max-width", maxWidth )
            , ( "background", "orange" )
            , ( "border-radius", "4px" )
            , ( "justify-content", "center" )
            , ( "flex", "1" )
            ]


styleRowBox { padding, size } =
    let
        w =
            (toString size) ++ "px"

        h =
            (toString size) ++ "px"

        m =
            (toString padding) ++ "px"
    in
        style
            [ ( "background", "yellow" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            , ( "border-radius", "4px" )
            , ( "margin", m )
            , ( "width", w )
            , ( "height", h )
            ]
