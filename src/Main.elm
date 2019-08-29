-- for debugging
import Debug

-- utlity functions
import List.Extra as List -- not sure if this is kosher

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
          |> verifyAngles 
          |> verifyPolygon of
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
  let pairStrs = String.split "," input in
    let pairs = List.filterMap parseElementToLengthAnglePair pairStrs in -- map strings to length-angle pairs, drop unparseables
      if input == "" || List.length pairStrs == List.length pairs then   -- verify that all parsed successfully
        Ok pairs
      else
        Err "Invalid syntax: Expecting a comma-separated list of length:angle pairs"

cleanInput : String -> String
cleanInput input = String.replace " " "" input

parseElementToLengthAnglePair : String -> Maybe (Float, Float)
parseElementToLengthAnglePair element =
  let lengthAndAngleStrs = String.split ":" element in
    let lengthAndAngleFloats = List.filterMap String.toFloat lengthAndAngleStrs in
      if List.length lengthAndAngleFloats == 2 then
        case List.head lengthAndAngleFloats of
          Just a ->
            case List.head (List.reverse lengthAndAngleFloats) of
              Just b -> Just (a, degrees b)
              Nothing -> Nothing
          Nothing -> Nothing
      else
        Nothing
      
-- verify a list of angles is valid based on the formula Total angle = 180*(n-2)  
verifyAngles : Result String (List (Float, Float)) -> Result String (List (Float, Float))
verifyAngles tile = 
  case tile of
    Ok lengthAnglePairs ->
      let angles = List.map (\(a, b) -> b) lengthAnglePairs in
        if List.length angles == 0 || floatEq (pi * (toFloat ((List.length angles) - 2))) (List.foldl (+) 0 angles) then
          tile -- pass along the tile if it has valid angles
        else
          Err "Angle validation failed. Sum of angles must be (number of sides - 2) * 180"
    Err msg -> Err msg

-- eliminates a triangle from tile and calls recursively
verifyPolygon : Result String (List (Float, Float)) -> Result String (List (Float, Float))
verifyPolygon tile = 
  case tile of
    Ok lengthAnglePairs -> 
      if List.length lengthAnglePairs > 3 then -- recursive call with polygon - 1 triangle
        case tile
          |> removeTriangle
          |> verifyPolygon of
          Ok _ -> tile
          Err msg -> Err msg
      else -- base case
        verifyTriangle tile
        
    Err msg -> Err msg
    
removeTriangle : Result String (List (Float, Float)) -> Result String (List (Float, Float))
removeTriangle tileResult =
  case tileResult of
    Ok tile ->
      let (lengths, angles) = List.unzip tile in
        case List.getAt 0 lengths of
          Just sideA -> 
            case List.getAt 1 lengths of
              Just sideB ->
                case List.getAt 0 angles of
                  Just angleC ->
                    let (angleB, angleA) = getMissingAngles sideA sideB angleC in
                      case List.last angles of
                        Just adjacentAngleA ->
                          case List.getAt 1 angles of
                            Just adjacentAngleB ->
                              let newSide = getMissingSide sideA sideB angleC in
                                let newSides = (newSide, adjacentAngleB - angleB) :: (List.drop 2 tile) in
                                  let lastIdx = (List.length newSides) - 1 in
                                    Ok (List.updateAt lastIdx (\(length, angle) -> (length, angle - angleA)) newSides)
                            Nothing -> Err "Error removing triangle: could not get 2nd angle"
                        Nothing -> Err "Error removing triangle: angles is empty"
                  Nothing -> Err "Error removing triangle: could not get 1st angle"
              Nothing -> Err "Error removing triangle: could not get 2nd length"
          Nothing -> Err "Error removing triangle: could not get 1st length"
    Err msg -> Err msg
    
verifyTriangle : Result String (List (Float, Float)) -> Result String (List (Float, Float))
verifyTriangle tile =
  case tile of
    Ok lengthAnglePairs ->
      if List.length lengthAnglePairs == 3 then
        let (lengths, angles) = List.unzip lengthAnglePairs in
          let rotatedAngles = rotate 1 angles in
            let lengthRotatedAnglePairs = List.zip lengths rotatedAngles in
              let ratios = List.map (\(length, angle) -> (length / sin(angle))) lengthRotatedAnglePairs in
                let first = List.head ratios in
                  case first of
                    Nothing -> Err "Invalid state. Length == 3, but length:angle ratios is empty"
                    Just a ->
                      if List.all (floatEq a) ratios then
                        tile
                      else
                        let angleDegrees = List.map (((*) (180 / pi))) angles in
                          let (lengthStrs, angleStrs) = (List.map String.fromFloat lengths, List.map String.fromFloat angleDegrees) in
                            Err ("Invalid triangle. Sine rule does not hold for lengths: " ++ (String.join ", " lengthStrs) ++ " and angles: " ++ (String.join ", " angleStrs))
      else
        Err ("Invalid number of sides for a triangle: " ++ (String.fromInt (List.length lengthAnglePairs)))
    Err msg -> Err msg
    
-- take 2 sides and an angle to get missing angles
getMissingAngles :  Float -> Float -> Float ->(Float, Float)
getMissingAngles sideA sideB angleC =
  let sideC = getMissingSide sideA sideB angleC in
    let ratio = sin(angleC) / sideC in
      ( asin (Basics.min 1 (ratio*sideA)), asin (Basics.min 1 (ratio*sideB)) )
    
-- take 2 sides and an angle and calculate the missing side length
-- cosine rule: a^2 = b^2 + c^2 - 2bc*cos(A) => a = sqrt(b^2 + c^2 - 2bc*cos(A))
getMissingSide : Float -> Float -> Float -> Float
getMissingSide sideA sideB angle =
  sqrt( (sideA*sideA) + (sideB*sideB) - 2*sideA*sideB*cos(angle) )
  
rotate : Int -> List a -> List a
rotate a list =
  let rotation = remainderBy (List.length list) a in
    (List.drop rotation list) ++ (List.take rotation list)
  
floatEq : Float -> Float -> Bool
floatEq a b =
  (Debug.log "floatEq diff" (abs ((Debug.log "floatEq a" a) - (Debug.log "floatEq b" b)))) < 0.001

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