module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import DragEvents exposing (..)
import Styles


-- MODEL

type alias GamePosition =
    Int


-- Currently, not used...
type alias RowPosition =
    Int


type alias TeamName =
    String


type alias DDPacket =
    { team : Maybe TeamName
    , rowPosition : RowPosition
    , gamePosition : GamePosition
    }


type alias Game =
    { team : Maybe TeamName
    , favorite : Maybe Bool
    , winner : Maybe Bool
    , finalScore : Maybe Int
    }


type alias Match =
    { games : ( Maybe Game, Maybe Game )
    , gameDate : Maybe String
    , submitted : Maybe Bool
    }


type alias Model =
    { games : List Game
    , teams : List TeamName
    , matches : List Match
    , dragInfo : Maybe DDPacket
    }


type Pool = Active DDPacket | Available | Eliminate


model : Model
model =
    { games = []
    , teams = []
    , matches = []
    , dragInfo = Nothing
    }


-- UPDATE

type Msg
    = AddNewMatch
    | CancelMove
    | DropOn Pool
    | Move ( Maybe DDPacket )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNewMatch ->
            { model | matches = ( (++) [ {
                games = ( Just {
                    team = Nothing
                    , favorite = Nothing
                    , winner = Nothing
                    , finalScore = Nothing
                },
                Just {
                    team = Nothing
                    , favorite = Nothing
                    , winner = Nothing
                    , finalScore = Nothing
                } ),
                gameDate = Nothing,
                submitted = Nothing
            } ] model.matches ) } ! []

        CancelMove ->
            { model | dragInfo = Nothing } ! []

        DropOn msg ->
            case msg of
                Active ddPacket ->
                    case model.dragInfo of
                        Nothing ->
                            model ! []

                        Just dragInfo ->
                            let
                                oldDragInfo = ( Maybe.withDefault ( DDPacket ( Just "" ) -1 -1 ) model.dragInfo )
                                newDragInfo = { oldDragInfo | rowPosition = ddPacket.rowPosition, gamePosition = ddPacket.gamePosition }

                                newMatches = List.indexedMap ( \i match ->
                                    case ddPacket.rowPosition == i of
                                        True ->
                                            let
                                                game = if newDragInfo.gamePosition == 0 then Tuple.first match.games else Tuple.second match.games
                                                oldGame = Maybe.withDefault ( Game Nothing Nothing Nothing Nothing ) game
                                                newGame = { oldGame | team = dragInfo.team }

                                                m = if newDragInfo.gamePosition == 0
                                                    then { match | games = ( Just newGame, Tuple.second match.games ) }
                                                    else { match | games = ( Tuple.first match.games, Just newGame ) }
                                            in
                                                m
                                        False ->
                                            match
                                ) model.matches
                            in
                                { model |
                                    matches = newMatches
                                    , teams = List.filter ( \t -> t /= ( Maybe.withDefault "" dragInfo.team ) ) model.teams
                                    , dragInfo = Just newDragInfo
                                } ! []

                Available ->
                    case model.dragInfo of
                        Nothing ->
                            model ! []

                        Just dragInfo ->
                            case dragInfo.rowPosition == -1 of
                                -- Don't allow a drop on the original/source drop zone!
                                True ->
                                    -- Resetting this value will set the opacity back to 1 in `addTeam`.
                                    { model | dragInfo = Nothing } ! []

                                False ->
                                    case dragInfo.team of
                                        Nothing ->
                                            { model | dragInfo = Nothing } ! []

                                        Just team ->
                                            let
                                                newMatches = List.indexedMap ( \i match ->
                                                    case dragInfo.rowPosition == i of
                                                        True ->
                                                            let
                                                                m = if dragInfo.gamePosition == 0
                                                                    then { match | games = ( Nothing, Tuple.second match.games ) }
                                                                    else { match | games = ( Tuple.first match.games, Nothing ) }
                                                            in
                                                                m
                                                        False ->
                                                            match
                                                ) model.matches
                                            in
                                                { model |
                                                    matches = newMatches
                                                    , teams = (::) team model.teams
                                                    , dragInfo = Nothing
                                                } ! []

                Eliminate ->
                    model ! []


        Move msg ->
            case msg of
                Nothing ->
                    model ! []

                Just ddPacket ->
                    let
                        oldDragInfo = ( Maybe.withDefault ( DDPacket ( Just "" ) -1 -1 ) model.dragInfo )
                        newDragInfo = { oldDragInfo |
                            team = ddPacket.team
                            , rowPosition = ddPacket.rowPosition
                            , gamePosition = ddPacket.gamePosition
                        }
                    in
                        { model | dragInfo = Just newDragInfo } ! []


-- VIEW

view : Model -> Html Msg
view model =
    let
        dragEvents = [ attribute "ondragover" "return false"
        , onDrop <| DropOn Available
        ]
    in
        div []
        [ div [ style Styles.dropZone ]
            [ table []
                ( (++)
                    [ thead []
                        [ tr []
                            [ th [] [ text "Game Date/Time" ]
                            , th [] [ text "Favorite" ]
                            , th [] [ text "Winner" ]
                            , th [] [ text "Final Score" ]
                            ]
                        ]
                    ]
                    ( model.matches |> List.indexedMap ( addMatch model ) )
                )
            ]
        , button [ style Styles.addGameBtn, onClick AddNewMatch ] [ text "ADD GAME" ]
        , div ( (::) ( style Styles.dropZone ) dragEvents )
            [ table []
                [ thead []
                    [ tr []
                        [ th [ colspan 4 ] [ text "Available Teams" ]
                        ]
                    ]
                , tbody []
                    ( List.map ( addTeam model ) <| model.teams )
                ]
            ]
        ]


addMatch : Model -> Int -> Match -> Html Msg
addMatch model rowId match =
    let
        firstGame =
            Tuple.first match.games
                |> Maybe.withDefault
                    { team = Nothing
                    , favorite = Nothing
                    , winner = Nothing
                    , finalScore = Nothing
                    }
        secondGame =
            Tuple.second match.games
                |> Maybe.withDefault
                    { team = Nothing
                    , favorite = Nothing
                    , winner = Nothing
                    , finalScore = Nothing
                    }

        commonStyles = [
            ( "display", "inline-block" ), ( "width", "100px" )
        ]

        events : Maybe TeamName -> GamePosition -> List ( Attribute Msg )
        events team gamePosition =
            [ attribute "draggable" "true"
            , onDragStart <| Move ( Just <| DDPacket team rowId gamePosition )
            , attribute "ondragstart"
                "event.dataTransfer.setData(\"text/plain\", \"derp\")"

            , attribute "ondragover" "return false"
            , onDrop <| DropOn ( DDPacket Nothing rowId gamePosition |> Active )
            ]
    in
        tbody []
            [ events firstGame.team 0 |> createMatchRow firstGame.team [] commonStyles
            , events secondGame.team 1 |> createMatchRow secondGame.team [] commonStyles
            ]


--TODO Cleanup
addTeam : Model -> TeamName -> Html Msg
addTeam model team =
    let
        commonDragEvents =
            [ attribute "draggable" "true"
              , onDragStart <| Move ( Just <| DDPacket ( Just team ) -1 -1 )
              , attribute "ondragstart" "event.dataTransfer.setData(\"text/plain\", \"derp\")"
            ]

        ( allStyles, dragEvents ) =
            case model.dragInfo of
                Nothing ->
                    (
                        [ ( "background", "#fff" ), ( "border", "1px solid #000" ), ( "opacity", "1" ) ]
                        , commonDragEvents
                    )
                Just dragInfo ->
                    if ( Maybe.withDefault "" dragInfo.team ) == team then
                        (
                            [ ( "opacity", "0.2" ) ]
                            , [ attribute "draggable" "true"
                              , onDragEnd <| CancelMove
                            ]
                        )
                    else
                        (
                            []
                            , commonDragEvents
                        )
    in
--        createRow ( Just team ) rowStyles cellStyles events
        [ text ( Maybe.withDefault "Drop team here" ( Just team ) ) ]
            |> div ( [ style allStyles ] ++ dragEvents )


createMatchRow : Maybe TeamName -> List ( String, String ) -> List ( String, String ) -> List ( Attribute msg ) -> Html msg
createMatchRow team rowStyles cellStyles events =
    tr ( [ style rowStyles ] ++ events )
        [ td [ style cellStyles ] [ text ( Maybe.withDefault "Drop team here" team ) ]
        , td [] [ input [ type_ "checkbox" ] [] ]
        , td [] [ input [ type_ "checkbox" ] [] ]
        , td [] [ input [ type_ "text" ] [] ]
        ]


init : ( Model, Cmd Msg )
init =
    { model | teams =
        [ "John"
        , "Paul"
        , "George"
        , "Ringo"
    ] } ! []


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


