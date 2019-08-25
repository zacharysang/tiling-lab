-- for debugging
import Debug

-- for rendering the view
import Browser
import Html exposing (Html, button, div, text, p, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

-- extra behavior for click events
import Html.Events.Extra.Mouse as Mouse

main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

-- SUBSCRIPTIONS (unused)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- MODEL

type alias Model = {
    cleanedInput: String,
    tile: List (Float, Float), -- consider defining this as its own type alias
    textWidth: Int,
    isResizing: Bool,
    inputError: Maybe String
  }

init : () -> (Model, Cmd Msg)
init _ = (
    {
      cleanedInput = "",
      tile = [],
      textWidth = 300,
      isResizing = False,
      inputError = Nothing
    },
    Cmd.none
  )


-- UPDATE

type Msg = SetTile String 
            | SetTextWidth Float 
            | StartResizing
            | StopResizing
            | Resize (Float, Float)

-- update will take a Model (the current one), and a new Shape, and output the new Shape
update : Msg -> Model -> (Model, Cmd Msg)
update msg currModel = 
  case msg of
    SetTile text ->
      let cleanedInput = cleanInput text in
        case cleanedInput 
          |> parseInputToTile
          |> verifyAngles of
          Ok tile -> ({
            currModel | 
              tile = tile, 
              cleanedInput = cleanedInput, 
              inputError = Nothing},
            Cmd.none)
          Err errMsg -> ({currModel | cleanedInput = cleanedInput, inputError = Just errMsg}, Cmd.none)
    SetTextWidth width -> (currModel, Cmd.none)
    StartResizing -> ({currModel | isResizing = True}, Cmd.none)
    StopResizing -> ({currModel | isResizing = False}, Cmd.none)
    Resize (x, y) -> 
      if currModel.isResizing then
        ({currModel | textWidth = (floor x)}, Cmd.none)
      else
        (currModel, Cmd.none) -- noop when not in resizing state
        
parseInputToTile : String -> Result String (List (Float, Float))
parseInputToTile input =
  let angleStrs = String.split "," input in
    let angles = List.filterMap parseElementToAngleLengthPair angleStrs in -- map strings to length-angle pairs, drop unparseables
      if input == "" || List.length angleStrs == List.length angles then   -- verify that all parsed successfully
        Ok angles
      else
        Err "Invalid syntax: Expecting a comma-separated list of angle:length pairs"

cleanInput : String -> String
cleanInput input = String.replace " " "" input

parseElementToAngleLengthPair : String -> Maybe (Float, Float)
parseElementToAngleLengthPair element =
  let angleAndLengthStrs = String.split ":" element in
    let angleAndLengthFloats = List.filterMap String.toFloat angleAndLengthStrs in
      if List.length angleAndLengthFloats == 2 then
        case List.head angleAndLengthFloats of
          Just a ->
            case List.head (List.reverse angleAndLengthFloats) of
              Just b -> Just (a, b)
              Nothing -> Nothing
          Nothing -> Nothing
      else
        Nothing
        
verifyAngles : Result String (List (Float, Float)) -> Result String (List (Float, Float))
verifyAngles tile = 
  case tile of
    Ok lengthAnglePairs ->
      let angles = List.map (\(a, b) -> b) lengthAnglePairs in
        if List.length angles == 0 || abs((toFloat (180 * ((List.length angles) - 2))) - (List.foldl (+) 0 angles)) < 0.0001 then
          tile -- pass along the tile if it has valid angles
        else
          Err "Angle validation failed. Sum of angles must be (number of sides - 2) * 180"
    Err msg -> Err msg

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
            textarea [onInput SetTile] []
          ],
          div [ class "horizontal-gutter"] [],
          div [ class "source" ] [
            (status model)
          ]
        ],
        div [
          class "vertical-gutter",
          style "cursor" "ew-resize",
          Mouse.onDown (\event -> StartResizing)] [],
        div [class "plane"] [
          p [] [text ("model.textWidth: " ++ (String.fromInt model.textWidth))],
          p [] [text ("input (cleaned): " ++ model.cleanedInput)]
        ]
      ]

status : Model -> Html Msg
status model =
  case model.inputError of
    Just msg ->
      div [ class "status",
            style "background-color" "red"]
          [text msg]
    Nothing ->
      div [ class "status",
            style "background-color" "green"]
          [text ""]