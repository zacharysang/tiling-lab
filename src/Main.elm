-- for debugging
import Debug

-- utlity functions
import List.Extra as List -- not sure if this is kosher
import Canvas
import Canvas.Settings
import Canvas.Settings.Line
import Color

-- for rendering the view
import Browser
import Html exposing (Html, button, div, text, p, textarea, canvas)
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
    tile: List (Float, Float), -- consider defining this as its own type and break it out
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
cleanInput input = 
  input
  |> String.replace " " ""
  |> String.replace "\n" ""
  |> String.replace "\t" ""

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
      if List.length lengthAnglePairs == 0 then
        tile
      else
        if List.length lengthAnglePairs > 3 then -- recursive call with polygon - 1 triangle
          case tile
              |> removeTriangle
              |> rotateTile 1 -- this fixes issue when scaling number of sides
              |> verifyPolygon of
            Ok _ -> tile
            Err msg -> Err msg
        else -- base case
          verifyTriangle tile
    Err msg -> Err msg
    
-- TODO include logic to skip over angles that are >= 180
-- since these will remove triangles that are external to the tile
removeTriangle : Result String (List (Float, Float)) -> Result String (List (Float, Float))
removeTriangle tileResult =
  case tileResult of
    Ok tile ->
      let (lengths, angles) = List.unzip tile in
        case List.getAt 0 angles of
          Just angleC -> -- can put check for angle here
            if angleC < pi then
              case List.getAt 0 lengths of
                Just sideA -> 
                  case List.getAt 1 lengths of
                    Just sideB ->
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
                    Nothing -> Err "Error removing triangle: could not get 2nd length"
                Nothing -> Err "Error removing triangle: could not get 1st length"
            else -- handle the convex case here
              Err "Error removing triangle: cannot handle concave shapes (yet)"
          Nothing -> Err "Error removing triangle: could not get 1st angle"
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
                        let angleDegrees = List.map toDegrees angles in
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
  
rotateTile : Int -> Result String (List (Float, Float)) -> Result String (List (Float, Float))
rotateTile i tile =
  case tile of
    Ok lengthAnglePairs ->
      Ok (rotate i lengthAnglePairs)
    Err msg -> Err msg
  
rotate : Int -> List a -> List a
rotate a list =
  let rotation = remainderBy (List.length list) a in
    (List.drop rotation list) ++ (List.take rotation list)
  
floatEq : Float -> Float -> Bool
floatEq a b =
  (abs (a - b)) < 0.001

toDegrees : Float -> Float
toDegrees radians =
  180 * (radians / pi)
    
-- wraps a float around another (mod for floats)
wrapFloat : Float -> Float -> Float
wrapFloat base target =
  if target > base || floatEq base target then
    wrapFloat base (target - base)
  else
    target

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
          p [] [text ("input (cleaned): " ++ model.cleanedInput)],
          Canvas.toHtml ( 600, 600 )
            [ id "canvas"]
            [
              Canvas.shapes [Canvas.Settings.fill Color.white] [ Canvas.rect (0, 0) 600 600 ],
              Canvas.shapes [ Canvas.Settings.fill Color.blue,
                              Canvas.Settings.Line.lineWidth 4,
                              Canvas.Settings.stroke Color.black]
                            [tilePath model.tile]
              --Canvas.shapes [Canvas.Settings.fill Color.red] (tileVertices model.tile),
              --Canvas.shapes [Canvas.Settings.fill Color.blue] [Canvas.circle (300,300) 5]
            ]
        ]
      ]

-- get a Canvas path shape from the given tile 
tilePath : List (Float, Float) -> Canvas.Shape
tilePath tile =
  let lengthAnglePairs = List.map (\(length, angleR) -> (length, toDegrees angleR)) tile in
    let startPt = (300, 300) in
      let points = (tilePoints (0, startPt) tile []) in
        Canvas.path startPt ((List.map Canvas.lineTo points) ++ [Canvas.lineTo startPt])
  
-- draw circles at each vertex of a tile
tileVertices : List (Float, Float) -> List Canvas.Shape
tileVertices tile = 
  List.map (\(x, y) -> Canvas.circle (x,y) 5) (tilePoints (0, (300,300)) tile [])

-- get list of (x, y) coords from an initial (angle, (coordPair)), and list of lengthAnglePairs
tilePoints : (Float, (Float, Float)) -> List (Float, Float) -> List (Float, Float) -> List (Float, Float)
tilePoints (startAngle, (startX, startY)) lengthAnglePairs acc =
  case List.head lengthAnglePairs of
    Just lengthAnglePair ->
      let (angle, (x, y)) = nextPoint (startAngle, (startX, startY)) lengthAnglePair in
        case List.tail lengthAnglePairs of
          Just tail ->
            tilePoints (angle, (x, y)) tail ((x,y) :: acc)
          Nothing ->
            acc
    Nothing -> acc -- this should never occur because of the above base case

-- takes an initial angle, a starting point and a lengthAnglePair, and returns a angle and point
nextPoint : (Float, (Float, Float)) -> (Float, Float) -> (Float, (Float, Float))
nextPoint (a, (x, y)) (length, angle) =
  let newAngle = (a + (pi - angle)) in -- here we need 'pi - angle' because turns are on the convex side, whereas input is interior angles
    let (vecX, vecY) = fromPolar (length, newAngle) in
      (newAngle, (x + vecX, y + vecY))
  

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
          
  