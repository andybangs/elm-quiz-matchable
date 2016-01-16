module Quiz where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp

import String exposing (length)

-- MODEL --

type QuizState = Start | Playing | Paused | End

type alias Value = String
type alias Selected = Bool

type alias Item = (Value, Selected)

type alias Matchable =
  { items: List Item
  , matched: Bool
  }

type alias Model =
  { title : String
  , description: String
  , columns: Int
  , data: List Matchable
  , score: Int
  , guessesRemaining: Int
  , state: QuizState
  }

item : Value -> Item
item val =
  (val, False)

matchable : List Item -> Matchable
matchable itemsList =
  { items = itemsList
  , matched = False
  }

initialModel : Model
initialModel =
  { title = "Matchable Quiz"
  , description = "A quiz app based on Sporcle's multi-column match quiz"
  , columns = 2
  , data =
    [ matchable [item "Monday", item "lunes"]
    , matchable [item "Tuesday", item "martes"]
    , matchable [item "Wednesday", item "miércoles"]
    , matchable [item "Thursday", item "jueves"]
    , matchable [item "Friday", item "viernes"]
    , matchable [item "Saturday", item "sábado"]
    , matchable [item "Sunday", item "domingo"]
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
    values : List (List Value)
    values =
      List.map (\matchable -> matchable.items) model.data
        |> List.map List.unzip
        |> List.map fst

    -- TODO: Map over values for dynamic generation of columns rather than
    -- assuming length of two and using List.take & List.drop

    left : List Html
    left =
      List.concatMap (\val -> List.take 1 val) values
        |> List.map (\val -> li [ class "tile" ] [ text val ])

    right : List Html
    -- TODO: Find a way to shuffle list instead of sorting by length
    right =
      List.concatMap (\val -> List.drop 1 val) values
        |> List.sortBy String.length
        |> List.map (\val -> li [ class "tile" ] [ text val ])


  in
    div [ id "container" ]
      [ h1 [ ] [ text "Quiz!" ]
      , h3 [ ] [ text ("This is a " ++ (toString model.columns) ++ " column quiz.") ]
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
