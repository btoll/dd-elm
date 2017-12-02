module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import DragEvents exposing (..)
import Styles


-- MODEL

type alias Game =
    { team : Maybe String
    , favorite : Maybe Bool
    , winner : Maybe Bool
    , finalScore : Maybe Int
    }


type alias Match =
    { games : ( Maybe Game, Maybe Game )
    , gameDate : Maybe String
    , submitted : Maybe Bool
    }


type alias TeamName =
    String


type alias Model =
    { games : List Game
    , teams : List TeamName
    , matches : List Match
    , movingTeam : Maybe TeamName
    }


--TODO
--type Pool = Active | Available | Eliminated


model : Model
model =
    { games = []
    , teams = []
    , matches = []
    , movingTeam = Nothing
    }


type alias DDPacket =
    { rowPosition : Int
    , gamePosition : Int
    }


-- UPDATE

type Msg
    = AddNewMatch
    | CancelMove
    | DropOn DDPacket
    | Move TeamName


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNewMatch ->
            ( { model | matches = ( (++) [ {
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
            } ] model.matches ) }, Cmd.none )

        CancelMove ->
            ( { model | movingTeam = Nothing }, Cmd.none )

        --TODO: Cleanup.
        DropOn ddPacket ->
            let
                newMatches = List.indexedMap ( \i match ->
                    case ddPacket.rowPosition == i of
                        True ->
                            let
                                game = if ddPacket.gamePosition == 0 then Tuple.first match.games else Tuple.second match.games
                                oldGame = Maybe.withDefault ( Game Nothing Nothing Nothing Nothing ) game
                                newGame = { oldGame | team = model.movingTeam }

                                m = if ddPacket.gamePosition == 0
                                    then { match | games = ( Just newGame, Tuple.second match.games ) }
                                    else { match | games = ( Tuple.first match.games, Just newGame ) }
                            in
                                m
                        False ->
                            match
                ) model.matches

                teams = List.filter ( \t -> t /= ( Maybe.withDefault "" model.movingTeam ) ) model.teams
            in
                ( { model |
                    matches = newMatches
                    , teams = teams
                }, Cmd.none )

        Move team ->
            ( { model | movingTeam = Just team }, Cmd.none )


-- VIEW

--TODO: Sort out the tbody generation.
view : Model -> Html Msg
view model =
    div []
    [ div [ style Styles.dropZone ]
        [ table []
            [ thead []
                [ tr []
                    [ th [] [ text "Game Date/Time" ]
                    , th [] [ text "Favorite" ]
                    , th [] [ text "Winner" ]
                    , th [] [ text "Final Score" ]
                    ]
            ]
            , table []
                ( List.indexedMap ( addMatch model ) <| model.matches )
            ]
        ]
--        [ table []
--            [ thead []
--                [ tr []
--                    [ th [] [ text "Game Date/Time" ]
--                    , th [] [ text "Favorite" ]
--                    , th [] [ text "Winner" ]
--                    , th [] [ text "Final Score" ]
--                    ]
--                    ]
--            , model.matches
--                |> List.indexedMap ( addMatch model )
--                |> List.head
--                |> Maybe.withDefault ( tbody [] [] )
--            ]
--        ]
    , button [ style Styles.addGameBtn, onClick AddNewMatch ] [ text "ADD GAME" ]
    , div [ style Styles.dropZone ]
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


--TODO: Clean this up.
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
    in
        tbody []
            [ tr [ attribute "ondragover" "return false", onDrop <| DropOn { rowPosition = rowId, gamePosition = 0 } ]
                [ td [ style commonStyles ] [ text ( Maybe.withDefault "Drop team here" firstGame.team ) ]
                , td [] [ input [ type_ "checkbox" ] [] ]
                , td [] [ input [ type_ "checkbox" ] [] ]
                , td [] [ input [ type_ "text" ] [] ]
                ]
            , tr [ attribute "ondragover" "return false", onDrop <| DropOn { rowPosition = rowId, gamePosition = 1 } ]
                [ td [ style commonStyles ] [ text ( Maybe.withDefault "Drop team here" secondGame.team ) ]
                , td [] [ input [ type_ "checkbox" ] [] ]
                , td [] [ input [ type_ "checkbox" ] [] ]
                , td [] [ input [ type_ "text" ] [] ]
                ]
            ]


addTeam : Model -> TeamName -> Html Msg
addTeam model team =
    let
        ( draggableRowStyles, draggableCellStyles, draggableEvents ) =
            case model.movingTeam of
                Nothing ->
                    (
                        [ ( "background", "#fff" ) ]
                        , []
                        , [ attribute "draggable" "true"
                          , onDragStart <| Move team
                          , attribute "ondragstart"
                              "event.dataTransfer.setData(\"text/plain\", \"derp\")"
                        ]
                    )
                Just movingTeam ->
                    if movingTeam == team then
                        (
                            []
                            , [ ( "opacity", "0.2" ) ]
                            , [ attribute "draggable" "true"
                              , onDragEnd <| CancelMove
                            ]
                        )
                    else
                        ( [], [], [] )
    in
        tr ( [ style draggableRowStyles ] ++ draggableEvents )
            [ td [ style draggableCellStyles ] [ text team ]
            , td [ style Styles.hidden ] [ input [ type_ "checkbox" ] [] ]
            , td [ style Styles.hidden ] [ input [ type_ "checkbox" ] [] ]
            , td [ style Styles.hidden ] [ input [ type_ "text" ] [] ]
            ]


init : ( Model, Cmd Msg )
init =
    ( { model | teams =
        [ "John"
        , "Paul"
        , "George"
        , "Ringo"
    ] }, Cmd.none )


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


