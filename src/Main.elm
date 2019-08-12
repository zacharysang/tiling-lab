-- for debugging
import Debug

-- for rendering the view
import Browser
import Html exposing (Html, button, div, text, p, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

-- extra behavior for click events
import Html.Events.Extra.Mouse as Mouse

main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- SUBSCRIPTIONS (unused)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL

type alias Model = {
  tile: List Float,
  textWidth: Int,
  isResizing: Bool}

init : () -> (Model, Cmd Msg)
init _ = (
    {
      tile = [],
      textWidth = 300,
      isResizing = False
    },
    Cmd.none
  )


-- UPDATE

type Msg = SetTile (List Float) 
            | SetTextWidth Float 
            | StartResizing
            | StopResizing
            | Resize (Float, Float)

-- update will take a Model (the current one), and a new Shape, and output the new Shape
update : Msg -> Model -> (Model, Cmd Msg)
update msg currModel = 
  case msg of
    SetTile tile -> (currModel, Cmd.none)
    SetTextWidth width -> (currModel, Cmd.none)
    StartResizing -> ({currModel | isResizing = True}, Cmd.none)
    StopResizing -> ({currModel | isResizing = False}, Cmd.none)
    Resize (x, y) -> 
      if currModel.isResizing then
        ({currModel | textWidth = (floor x)}, Cmd.none)
      else
        (currModel, Cmd.none) -- noop when not in resizing state

-- VIEW

view : Model -> Html Msg -- right now message is a dummy since the produced html never emits messages
view model =
  div [ class "container",
        Mouse.onUp (\event -> StopResizing),
        Mouse.onMove (\event -> Resize event.screenPos)]
      [ 
        div [ class "sidebar",
            style "width" ((String.fromInt model.textWidth) ++ "px")]
        [
          div [ class "source" ] [
            textarea [] []
          ],
          div [ class "horizontal-gutter"] [],
          div [ class "source" ] []
        ],
        div [
          class "vertical-gutter",
          style "cursor" "ew-resize",
          Mouse.onDown (\event -> StartResizing)] [],
        div [class "plane"] [
          p [] [text ("model.textWidth: " ++ (String.fromInt model.textWidth))],
          p [] [text ("model.tile: [" ++ String.join "," (List.map String.fromFloat model.tile) ++ "]")]
        ]
      ]