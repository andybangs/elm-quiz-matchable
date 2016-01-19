module Quiz where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import Debug

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

type alias Data =
  List Matchable

type alias Model =
  { title : String
  , description: String
  , data: Data
  , score: Int
  , wrong: Int
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
  , score = 0
  , wrong = 0
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
  | Select Int Int Bool Bool

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

    Select mid id selected matched ->
      let
        -- model.data --
        deselectItem : Item -> Item
        deselectItem i =
          if id /= i.id then i
          else { i | selected = False }

        selectItem : Item -> Item
        selectItem i =
          if id /= i.id then i
          else { i | selected = True }

        updateMatched : List Item -> Bool -> Bool
        updateMatched items matched =
          let
            selected =
              List.map (\i -> i.selected) items
          in
            if List.member False selected then matched
            else True

        updateMatchable : Matchable -> Matchable
        updateMatchable m =
          if mid /= m.id then
            let
              updatedItems = List.map deselectItem m.items
            in
              { m | items = updatedItems }
          else
            let
              updatedItems = List.map selectItem m.items
            in
              { m |
                items = updatedItems
              , matched = updateMatched updatedItems m.matched
              }

        numSelected : Data -> Int
        numSelected data =
          List.concatMap (\m -> m.items) data
            |> List.filter (\i -> i.selected == True)
            |> List.length

        updateData : Data -> Data
        updateData data =
          let
            updatedData : Data
            updatedData =
              List.map updateMatchable data

            isAnswer : Bool
            isAnswer =
              (numSelected updatedData) == 2

            deselectAllItems : Matchable -> Matchable
            deselectAllItems m =
              { m | items = List.map (\i -> { i | selected = False }) m.items }
          in
            if isAnswer then List.map deselectAllItems updatedData
            else updatedData

        newData : Data
        newData = updateData model.data

        -- model.score --
        calcScore : Data -> Int
        calcScore ms =
          List.filter (\m -> m.matched == True) ms
            |> List.length

        newScore : Int
        newScore = calcScore newData

        -- model.wrong --
        calcWrong : Data -> (Int, Int) -> Int -> Int
        calcWrong data sTup w =
          let
            oldScore = fst sTup
            newScore = snd sTup

            isAnswer =
              (numSelected data) == 2 || (numSelected data) == 0
          in
            if isAnswer && newScore == oldScore then w + 1
            else w


        newWrong : Int
        newWrong = calcWrong newData (model.score, newScore) model.wrong

      in
        if matched then
          model
        else
          { model |
            data = newData
          , score = newScore
          , wrong = newWrong
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
    tile : Int -> Int -> String -> Bool -> Bool -> Html
    tile mid id value selected matched =
      li
        [ classList [ ("tile", True), ("highlight", selected), ("matched", matched) ]
        , onClick address (Select mid id selected matched)
        ]
        [text value]

    -- TODO: Shuffle lists

    buildTile : Matchable -> Int -> List Html
    buildTile matchable id =
      List.filter (\item -> item.id == id) matchable.items
        |> List.map (\item -> tile item.mid item.id item.value item.selected matchable.matched)

    left : List Html
    left =
      List.concatMap (\matchable -> buildTile matchable 1) model.data

    right : List Html
    right =
      List.concatMap (\matchable -> buildTile matchable 2) model.data

  in
    div [ id "container" ]
      [ h1 [ ] [ text "Quiz!" ]
      , h3 [ ] [ text ("State: " ++ (toString model.state)) ]
      , h3 [ ] [ text ("Correct: " ++ (toString model.score)) ]
      , h3 [ ] [ text ("Wrong: " ++ (toString model.wrong)) ]
      , h3 [ ] [ text ("Guesses Remaining: " ++ (toString ((List.length model.data) - model.score - model.wrong))) ]
      -- , h3 [ ] [ text ("Data: " ++ (toString model.data)) ]
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
