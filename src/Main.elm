module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Cancel
    | Save
    | DeletePlay Play


type PlayerMode
    = EditMode
    | ReadMode


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input name ->
            Debug.log "Updated: "
                { model | name = name }

        Save ->
            if String.isEmpty model.name then
                model
            else
                save model

        Cancel ->
            { model | name = "", playerId = Nothing }

        Score player points ->
            score model player points

        Edit player ->
            { model | name = player.name, playerId = Just player.id }

        DeletePlay play ->
            deletePlay model play


deletePlay : Model -> Play -> Model
deletePlay model play =
    let
        newPlays =
            List.filter (\p -> p.id /= play.id) model.plays

        newPlayers =
            List.map
                (\player ->
                    if player.id == play.playerId then
                        { player | points = player.points - 1 * play.points }
                    else
                        player
                )
                model.players
    in
        { model | plays = newPlays, players = newPlayers }


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player | points = player.points + points }
                    else
                        player
                )
                model.players

        play =
            Play (List.length model.plays) scorer.id scorer.name points
    in
        { model | players = newPlayers, plays = play :: model.plays }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


add : Model -> Model
add model =
    let
        player =
            Player (List.length model.players) model.name 0
    in
        { model | players = player :: model.players, name = "", playerId = Nothing }


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.name }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.id == id then
                        { play | name = model.name }
                    else
                        play
                )
                model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , name = ""
            , playerId = Nothing
        }


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Counter" ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playerForm : Model -> Html Msg
playerForm model =
    let
        className =
            if model.name == "" then
                ""
            else
                "blue"
    in
        Html.form [ onSubmit Save ]
            [ input
                [ type_ "text"
                , placeholder "Add/Edit Player ..."
                , onInput Input
                , value model.name
                , class className
                ]
                []
            , button [ type_ "submit" ] [ text "Save" ]
            , button [ type_ " button", onClick Cancel ] [ text "Cancel" ]
            ]


playerView : Maybe Int -> Player -> Html Msg
playerView id player =
    let
        className =
            case id of
                Just id ->
                    if id == player.id then
                        "blue"
                    else
                        ""

                Nothing ->
                    ""
    in
        li [ class "player" ]
            [ i
                [ class "edit"
                , onClick (Edit player)
                ]
                []
            , div
                [ class className
                ]
                [ text player.name ]
            , button
                [ type_ "button"
                , onClick (Score player 2)
                ]
                [ text "2pt" ]
            , button
                [ type_ "button"
                , onClick (Score player 3)
                ]
                [ text "3pt" ]
            , div []
                [ text (toString player.points) ]
            ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map (playerView model.playerId)
        |> ul []


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , playerTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playSection : Model -> Html Msg
playSection model =
    div []
        [ playerListHeader
        , playList model.plays
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


playList : List Play -> Html Msg
playList plays =
    plays
        |> List.sortBy .name
        |> List.map playView
        |> ul []


playView : Play -> Html Msg
playView play =
    li []
        [ i
            [ class "remove"
            , onClick (DeletePlay play)
            ]
            []
        , div [] [ text play.name ]
        , div [] [ text (toString play.points) ]
        ]


playerTotal : Model -> Html Msg
playerTotal model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (toString total) ]
            ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view
        , update = update
        , model = initModel
        }
