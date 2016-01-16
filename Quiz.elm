module Quiz where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp

-- MODEL --

type QuizState = Start | Playing | Paused | End

type alias Item =
  { id: Int
  , value: String
  , selected: Bool
  }

type alias Matchable =
  { id: Int
  ,items: List Item
  , matched: Bool
  }

type alias Model =
  { title : String
  , description: String
  , data: List Matchable
  , score: Int
  , guessesRemaining: Int
  , state: QuizState
  }

item : Int -> String -> Item
item id value =
  { id = id
  , value = value
  , selected = False
  }

matchable : Int -> List Item -> Matchable
matchable id itemsList =
  { id = id
  , items = itemsList
  , matched = False
  }

initialModel : Model
initialModel =
  { title = "Matchable Quiz"
  , description = "A quiz app based on Sporcle's multi-column match quiz"
  , data =
    [ matchable 1 [item 1 "Monday", item 2 "lunes"]
    , matchable 2 [item 1 "Tuesday", item 2 "martes"]
    , matchable 3 [item 1 "Wednesday", item 2 "miércoles"]
    , matchable 4 [item 1 "Thursday", item 2 "jueves"]
    , matchable 5 [item 1 "Friday", item 2 "viernes"]
    , matchable 6 [item 1 "Saturday", item 2 "sábado"]
    , matchable 7 [item 1 "Sunday", item 2 "domingo"]
    ]
  , score = 0
  , guessesRemaining = List.length [1,2,3]
  , state = Start
  }


-- UPDATE --

type Action
  = NoOp
  | Play
  | Pause
  | Resume
  | Quit
  | Reset

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Play ->
      { model | state = Playing }

    Pause ->
      { model | state = Paused }

    Resume ->
      { model | state = Playing }

    Quit ->
      { model | state = End }

    Reset ->
      { model | state = Start }


-- VIEW --

startView address model =
  div [ id "container" ]
    [ h1 [ ] [ text model.title ]
    , h3 [ ] [ text model.description ]
    , h3 [ ] [ text ("State: " ++ (toString model.state)) ]
    , button
        [ onClick address Play ]
        [ text "Play" ]
    ]

playView address model =
  let
    tile : String -> Html
    tile value =
      li [ class "tile" ] [text value]

    -- TODO: Shuffle lists

    left : List Html
    left =
      List.map (\matchable -> matchable.items) model.data
        |> List.concatMap (List.filter (\item -> item.id == 1))
        |> List.map (\item -> tile item.value)

    right : List Html
    right =
      List.map (\matchable -> matchable.items) model.data
        |> List.concatMap (List.filter (\item -> item.id == 2))
        |> List.map (\item -> tile item.value)

  in
    div [ id "container" ]
      [ h1 [ ] [ text "Quiz!" ]
      , h3 [ ] [ text ("This is a 2 column matching quiz.") ]
      , h3 [ ] [ text ("State: " ++ (toString model.state)) ]
      , div
          [ class "row" ]
          [ div
              [ class "column" ]
              [ ul [ class "tile-cont" ] left ]
          , div
              [ class "column" ]
              [ ul [ class "tile-cont" ] right ]
          ]
      , button
          [ onClick address Pause ]
          [ text "Pause" ]
      ]

pauseView address model =
  div [ id "container" ]
    [ h1 [ ] [ text "Pause" ]
    , h3 [ ] [ text "The game is paused. Phew!" ]
    , h3 [ ] [ text ("State: " ++ (toString model.state)) ]
    , button
        [ onClick address Resume ]
        [ text "Resume" ]
    , button
        [ onClick address Quit ]
        [ text "Quit" ]
    ]

endView address model =
  div [ id "container" ]
    [ h1 [ ] [ text "Game Over" ]
    , h3 [ ] [ text "You failed miserably." ]
    , h3 [ ] [ text ("State: " ++ (toString model.state)) ]
    , button
        [ onClick address Reset ]
        [ text "Try Again!" ]
    ]

view : Address Action -> Model -> Html
view address model =
  case model.state of
    Start ->
      startView address model

    Playing ->
      playView address model

    Paused ->
      pauseView address model

    End ->
      endView address model


-- MAIN --

main : Signal Html
main =
  StartApp.start
    { model = initialModel
    , view = view
    , update = update
    }
