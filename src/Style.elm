module Style exposing (..)

import Html.Attributes exposing (..)


withFont =
    [ style "font-size" "12px"
    ]


screen =
    [ style "height" "100vh"
    , style "width" "100vw"
    , style "display" "flex"
    ]


pile =
    [ style "display" "flex"
    , style "flex-direction" "column"
    ]


panel =
    [ style "flex" "1"
    ]


button =
    [ style "padding" "1em"
    , style "border" "none"
    , style "border-radius" "0"
    ]


input =
    panel
        ++ [ style "border" "none"
           , style "margin" "1em"
           ]


output =
    [ style "flex" "2"
    ]
