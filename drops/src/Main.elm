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
import Html.Attributes exposing (class)

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

mkOpts n = map (\i -> "Option " ++ String.fromInt i) <| List.range 1 n

init : Model
init =
  let empty = { selection = [], visible = False }
  in { overall = empty, category1 = empty, category2 = empty, category3 = empty }

type Section = Overall | Category1 | Category2 | Category3
type Msg a = Select Section a | Deselect Section a | Show Section | Hide Section


main = Browser.sandbox { init = init, view = view, update = update}

view : Model -> Html (Msg String)
view model = div [] [
    viewCard Overall (\x -> x) (mkOpts 10) model.overall
  , viewCard Category1 (\x -> x) (mkOpts 13) model.category1
  , viewCard Category2 (\x -> x) (mkOpts 9) model.category2
  , viewCard Category3 (\x -> x) (mkOpts 12) model.category3
  ]

viewCard : Section -> (a -> String) -> List a -> CardModel a -> Html (Msg a)
viewCard section show opts model = div [] [
    div [ onClick (toggleDropdown model.visible section) ] [ text "Compare" ],
    if model.visible then viewDropdown section show opts model.selection else text "",
    div [] [text "Selection: ", selected show model]
  ]

toggleDropdown visible section = if visible then Hide section else Show section

viewDropdown : Section -> (a -> String) -> List a -> List a -> Html (Msg a)
viewDropdown section show opts selection =
  node "on-click-outside"
    [ class "dropdown", on "clickoutside" (Decode.succeed (Hide section)) ]
    [ div []  <| map (checkbox section show selection) opts ]

checkbox : Section -> (a -> String) -> List a -> a -> Html (Msg a)
checkbox section show selection o = div [] [
    input [type_ "checkbox", onCheck (toggle section o), checked (member o selection)] [],
    span [] [text <| show o]
  ]

toggle : Section -> a -> Bool -> Msg a
toggle section o c = if c then Select section o else Deselect section o

selected show model = text <| concat <| intersperse ", " (map show <| model.selection)

update : Msg String -> Model -> Model
update msg model = case msg of
   (Select s a) -> mapSection s (\m -> { m | selection=a::m.selection}) model
   (Deselect s a) -> mapSection s (\m -> { m | selection=filter (\b -> b /= a) m.selection }) model
   Show s -> mapSection s (\m -> { m | visible = True }) model
   Hide s -> mapSection s (\m -> { m | visible = False }) model

mapSection : Section -> (CardModel String -> CardModel String) -> Model -> Model
mapSection section f model = case section of
  Overall -> { model | overall=f model.overall}
  Category1 -> { model | category1=f model.category1}
  Category2 -> { model | category2=f model.category2}
  Category3 -> { model | category3=f model.category3}