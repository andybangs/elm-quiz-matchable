module Quiz where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp

-- MODEL --

type QuizState = Start | Playing | Paused | End

type alias Model =
  { title : String
  , description: String
  , columns: Int
  , state: QuizState
  }

initialModel : Model
initialModel =
  { title = "Matchable Quiz"
  , description = "A quiz app based on Sporcle's multi-column match quiz"
  , columns = 2
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
  div [ id "container" ]
    [ h1 [ ] [ text "Quiz!" ]
    , h3 [ ] [ text ("This is a " ++ (toString model.columns) ++ " column quiz.") ]
    , h3 [ ] [ text ("State: " ++ (toString model.state)) ]
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
