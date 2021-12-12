module Main exposing (main)

import Browser
import Html exposing (Html, div, input)
import Html.Events exposing (onClick, onCheck)
import Html.Attributes exposing (type_)
import Html exposing (text)
import List exposing (map)
import Html exposing (span)
import List exposing (intersperse)
import String exposing (concat)
import List exposing (filter)

type Model a = Selection (List a)

type Msg a = Select a | Deselect a

id x = x

options = ["Option 1", "Option 2", "Option 3"]

main = Browser.sandbox { init = Selection [], view = view id options, update = update}

view : (a -> String) -> List a -> Model a -> Html (Msg a)
view show opts sel = div [] [
    div []  <| map (checkbox show) opts,
    div [] [text "Selection: ", selected show sel]
  ]

checkbox : (a -> String) -> a -> Html (Msg a)
checkbox show o = div [] [
    input [type_ "checkbox", onCheck (toggle o)] [],
    span [] [text <| show o]
  ]

toggle o c = if c then Select o else Deselect o

selected show (Selection sel) = text <| concat <| intersperse ", " (map show sel)

update : Msg a -> Model a -> Model a
update msg (Selection ls) = case msg of
   (Select a) ->  Selection (a::ls)
   (Deselect a) -> Selection <| filter (\b -> b /= a) ls