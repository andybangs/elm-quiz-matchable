module Quiz where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp

-- MODEL --

type QuizState = Start | Playing | Paused | End

type alias Item =
  { mid: Int
  , id: Int
  , value: String
  , selected: Bool
  }

type alias Matchable =
  { id: Int
  , items: List Item
  , matched: Bool
  }

type alias Selected =
  List (Int, Int)

type alias Data =
  List Matchable

type alias Model =
  { title : String
  , description: String
  , data: Data
  , selected: Selected
  , score: Int
  , guessesRemaining: Int
  , state: QuizState
  }

item : Int -> Int -> String -> Item
item mid id value =
  { mid = mid
  , id = id
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
    [ matchable 1 [item 1 1 "Monday", item 1 2 "lunes"]
    , matchable 2 [item 2 1 "Tuesday", item 2 2 "martes"]
    , matchable 3 [item 3 1 "Wednesday", item 3 2 "miércoles"]
    , matchable 4 [item 4 1 "Thursday", item 4 2 "jueves"]
    , matchable 5 [item 5 1 "Friday", item 5 2 "viernes"]
    , matchable 6 [item 6 1 "Saturday", item 6 2 "sábado"]
    , matchable 7 [item 7 1 "Sunday", item 7 2 "domingo"]
    ]
  , selected = []
  , score = 0
  , guessesRemaining = 7
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
  | Select Int Int Bool

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
      initialModel

    Select mid id selected ->
      let
        deselectItem : Item -> Item
        deselectItem i =
          if mid /= i.mid && id /= i.id then i
          else { i | selected = False }

        selectItem : Item -> Item
        selectItem i =
          if id /= i.id then i
          else { i | selected = (not i.selected) }

        updateMatchable : Matchable -> Matchable
        updateMatchable m =
          if mid /= m.id then { m | items = List.map deselectItem m.items}
          else { m | items = List.map selectItem m.items }

        newData : Data
        newData = List.map updateMatchable model.data

        updateSelected : Data -> Selected
        updateSelected ms =
          List.filter (\m -> m.matched == False ) ms
            |> List.concatMap (\m -> m.items)
            |> List.filter (\i -> i.selected == True)
            |> List.map (\i -> (i.mid, i.id))
      in
        { model |
          data = newData
        , selected = updateSelected newData
        }


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
    tile : Int -> Int -> String -> Bool -> Html
    tile mid id value selected =
      li
        [ classList [ ("tile", True), ("highlight", selected) ]
        , onClick address (Select mid id selected)
        ]
        [text value]

    -- TODO: Shuffle lists

    left : List Html
    left =
      List.map (\matchable -> matchable.items) model.data
        |> List.concatMap (List.filter (\item -> item.id == 1))
        |> List.map (\item -> tile item.mid item.id item.value item.selected)

    right : List Html
    right =
      List.map (\matchable -> matchable.items) model.data
        |> List.concatMap (List.filter (\item -> item.id == 2))
        |> List.map (\item -> tile item.mid item.id item.value item.selected)

  in
    div [ id "container" ]
      [ h1 [ ] [ text "Quiz!" ]
      , h3 [ ] [ text ("State: " ++ (toString model.state)) ]
      , h3 [ ] [ text ("Selected: " ++ (toString model.selected)) ]
      -- , h3 [ ] [ text ("Score: " ++ (toString model.score)) ]
      -- , h3 [ ] [ text ("Guesses Remaining: " ++ (toString model.guessesRemaining)) ]
      , h3 [ ] [ text ("Data: " ++ (toString model.data)) ]
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
