import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = {
  tile: Shape,
  textHeight: Int,
  textWidth: Int }
type alias Shape = List Float -- represent a shape as a list of angles

init : Model
init = {
  tile = [1,2],
  textHeight = 200,
  textWidth = 200 }


-- UPDATE

type Msg = SetTile Shape | SetTextHeight Float | SetTextWidth Float

-- update will take a Model (the current one), and a new Shape, and output the new Shape
update : Msg -> Model -> Model
update msg currModel = 
  case msg of
    SetTile tile -> currModel
    SetTextHeight height -> currModel
    SetTextWidth width -> currModel
  


-- VIEW

view : Model -> Html msg -- right now message is a dummy since the produced html never emits messages
view model =
  div []
    [ div [ class "col1", width model.textWidth] [
        div [class "row1", height model.textHeight, style "background-color" "red"] [text (String.fromInt (List.length model.tile)) ],
        div [class "row2", style "background-color" "blue"] []
      ],
      div [ class "col2" ] []]