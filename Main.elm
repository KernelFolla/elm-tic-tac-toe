module Main where

import Array exposing (Array)
import Debug

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.Decoder exposing (..)
import StartApp.Simple as StartApp


-- model

type Action
  = PlaceX (Int, Int)
  | PlaceO (Int, Int)
  | Reset


type alias Cell
  = Maybe Player


type Player
  = X
  | O


type alias Board
  = Array (Array Cell)


emptyBoard : Board
emptyBoard =
  Array.repeat 3 (Array.repeat 3 Nothing)


-- update


update : Action -> Board -> Board
update action board =
  case action of
    PlaceX pos ->
      place (Just X) board pos

    PlaceO pos ->
      place (Just O) board pos

    Reset ->
      emptyBoard


place : Cell -> Board -> (Int, Int) -> Board
place cell board (rowIndex, colIdx) =
  let
    oldRow =
      Array.get rowIndex board
      |> Maybe.withDefault (Array.empty)

    newRow =
      Array.set colIdx cell oldRow
  in
    Array.set rowIndex newRow board


winner : Board -> Maybe Player
winner board =
  --Just X
  let
    lines =
      --rows
      [ [(0, 0), (0, 1), (0, 2)]
      , [(1, 0), (1, 1), (1, 2)]
      , [(2, 0), (2, 1), (2, 2)]
      -- columns
      , [(0, 0), (1, 0), (2, 0)]
      , [(0, 1), (1, 1), (2, 1)]
      , [(0, 2), (1, 2), (2, 2)]
      -- diagornals
      , [(0, 0), (1, 1), (2, 2)]
      , [(0, 2), (1, 1), (2, 0)]
      ]

    getAtCoords : (Int, Int) -> Cell
    getAtCoords (rowIdx, colIdx) =
      (Array.get rowIdx board)
        `Maybe.andThen` (\row -> Array.get colIdx row)
      |> getOrCrash ("invalid coordinates: " ++ toString (rowIdx, colIdx))
  in
    lines
      |> List.map (\[first, second, third] ->
            winnerForLine
              ( getAtCoords first
              , getAtCoords second
              , getAtCoords third
              )
          )
      |> List.filterMap identity
      |> List.head


winnerForLine : (Cell, Cell, Cell) -> Maybe Player
winnerForLine cells =
  case cells of
    (Just X, Just X, Just X) ->
      Just X

    (Just O, Just O, Just O) ->
      Just O

    _ ->
      Nothing


-- view


view : Signal.Address Action -> Board -> Html
view addr board =
  div
    []
    [ viewBoard addr board
    , viewStatus addr board
    ]


viewStatus : Signal.Address Action -> Board -> Html
viewStatus addr board =
  case winner board of
    Just player ->
      div
        []
        [ text ("Winner: " ++ toString player ++ "!  ")
        , button
            [ onClick addr Reset ]
            [ text "Reset" ]
        ]

    Nothing ->
      text "Playing..."


viewBoard : Signal.Address Action -> Board -> Html
viewBoard addr board =
  table
    []
    (Array.indexedMap (\rowIndex row -> viewRow addr rowIndex row) board
      |> Array.toList)


viewRow : Signal.Address Action -> Int -> Array Cell -> Html
viewRow addr rowIndex row =
  tr
    []
    (Array.indexedMap (\colIndex cell -> viewCell addr rowIndex colIndex cell) row
      |> Array.toList)


viewCell : Signal.Address Action -> Int -> Int -> Cell -> Html
viewCell addr rowIdx colIdx cell =
  let
    contents =
      case cell of
        Just X ->
          "X"

        Just O ->
          "O"

        Nothing ->
          "_"

    getMove : MouseEvent -> Action
    getMove mouseEvent =
      if mouseEvent.metaKey then
        PlaceO (rowIdx, colIdx)
      else
        PlaceX (rowIdx, colIdx)
  in
    td
      [ on "click" mouseEvent (\mouseEvent -> Signal.message addr (getMove mouseEvent)) ]
      [ text contents ]


getOrCrash : String -> Maybe a -> a
getOrCrash msg maybe =
  case maybe of
    Just x ->
      x

    Nothing ->
      Debug.crash msg


-- wiring

config : StartApp.Config Board Action
config =
  { model = emptyBoard
  , view = view
  , update = update
  }


main : Signal Html
main =
  StartApp.start config
