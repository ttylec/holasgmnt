module Main exposing (main)

import Browser
import Html exposing (Html, div, input)
import Html.Events exposing (onClick, onCheck, on)
import Html.Attributes exposing (type_)
import Html exposing (text)
import List exposing (map)
import Html exposing (span)
import List exposing (intersperse, member)
import String exposing (concat)
import List exposing (filter)
import Html exposing (node)
import Json.Decode as Decode
import Html.Attributes exposing (id)
import Html.Attributes exposing (checked)

type alias Model = {
    overall : CardModel String
  , category1 : CardModel String
  , category2 : CardModel String
  , category3 : CardModel String
  }

type alias CardModel a = {
    selection : (List a)
  , visible : Bool
  }

init opts = { selection = opts, visible = False }

type Msg a = Select a | Deselect a | Show | Hide

options = ["Option 1", "Option 2", "Option 3"]

main = Browser.sandbox { init = init [], view = viewCard (\x -> x) options, update = update}



viewCard : (a -> String) -> List a -> CardModel a -> Html (Msg a)
viewCard show opts model = div [] [
    div [ onClick Show ] [ text "Compare" ],
    if model.visible then viewDropdown show opts model.selection else text "",
    div [] [text "Selection: ", selected show model]
  ]

viewDropdown : (a -> String) -> List a -> List a -> Html (Msg a)
viewDropdown show opts selection =
  node "on-click-outside"
    [ id "dropdown", on "clickoutside" (Decode.succeed Hide) ]
    [ div []  <| map (checkbox show selection) opts ]

checkbox : (a -> String) -> List a -> a -> Html (Msg a)
checkbox show selection o = div [] [
    input [type_ "checkbox", onCheck (toggle o), checked (member o selection)] [],
    span [] [text <| show o]
  ]

toggle : a -> Bool -> Msg a
toggle o c = if c then Select o else Deselect o

selected show model = text <| concat <| intersperse ", " (map show <| model.selection)

update : Msg a -> CardModel a -> CardModel a
update msg model = case msg of
   (Select a) ->  { model | selection=a::model.selection}
   (Deselect a) -> { model | selection=filter (\b -> b /= a) model.selection }
   Show -> { model | visible = True }
   Hide -> { model | visible = False }