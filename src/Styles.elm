module Styles exposing (..)

type alias StyleList = List (String, String)

addGameBtn : StyleList
addGameBtn =
    [ ( "marginLeft", "20px" )
    ]

dropZone : StyleList
dropZone =
    [ ( "border", "2px solid black" )
    , ( "height", "230px" )
    , ( "margin", "20px" )
    , ( "overflow", "auto" )
    ]

hidden : StyleList
hidden =
    [ ( "visibility", "hidden" )
    ]

match : StyleList
match =
    [ ( "border", "2px solid green" )
    , ( "margin", "20px" )
    , ( "background", "blue" )
    ]

