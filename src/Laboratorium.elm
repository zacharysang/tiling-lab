module Laboratorium exposing (Msg(..), Model, init, update)

-- MODEL
type alias Model = {
    textWidth: Int,
    isResizing: Bool
  }

init : () -> Model
init _ = {
    textWidth = 300,
    isResizing = False
  }
  
-- UPDATE
type Msg =  StartResizing
            | StopResizing
            | Resize (Float, Float)

update : Msg -> Model -> Model
update msg currModel =
  case msg of
    StartResizing -> {currModel | isResizing = True}
    StopResizing -> {currModel | isResizing = False}
    Resize (x, y) -> 
      if currModel.isResizing then
        {currModel | textWidth = (floor x)}
      else
        currModel -- noop when not in resizing state