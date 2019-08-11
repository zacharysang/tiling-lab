import Browser
import Html exposing (Html, button, div, text, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- SUBSCRIPTIONS (unused)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL

type alias Model = {
  tile: List Float,
  textWidth: Int }

init : () -> (Model, Cmd Msg)
init _ = (
    {
      tile = [],
      textWidth = 300
    },
    Cmd.none
  )


-- UPDATE

type Msg = SetTile (List Float) | SetTextWidth Float

-- update will take a Model (the current one), and a new Shape, and output the new Shape
update : Msg -> Model -> (Model, Cmd Msg)
update msg currModel = 
  case msg of
    SetTile tile -> (currModel, Cmd.none)
    SetTextWidth width -> (currModel, Cmd.none)
  

-- VIEW

view : Model -> Html msg -- right now message is a dummy since the produced html never emits messages
view model =
  div [class "container"]
    [ 
     div [ class "sidebar",
            style "width" ((String.fromInt model.textWidth) ++ "px")]
      [
        div [ class "source" ] [],
        div [ class "horizontal-gutter" ] [],
        div [ class "source" ] []
      ],
      div [
        class "vertical-gutter",
        style "cursor" "ew-resize"] [],
      div [class "plane"] [
        p [] [text ("model.textWidth: " ++ (String.fromInt model.textWidth))],
        p [] [text ("model.tile: " ++ String.join "," (List.map String.fromFloat model.tile))]
      ]
    ]