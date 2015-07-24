module FollowMouse where

import Color exposing (green)
import DraggableForm as DF
import Graphics.Collage exposing (collage)
import Graphics.Element exposing (Element)
import Maybe exposing (withDefault)
import Mouse
import Signal exposing ((<~), (~))
import Touch
import Window


-- Model

type alias Model =
  { draggableForms : DF.Model
  }


initialState : Model
initialState =
  {
    draggableForms =
      { forms =
          [ DF.createCircle green 100 (50, 50) 0
          ]
      , selectedId = Just 0
      }
  }


-- Update

type Action
  = MouseMove (Int, Int)


update : Action -> Model -> Model
update action model =
  case action of
    MouseMove newPosition ->
      { model | draggableForms <- DF.update (DF.MoveSelected newPosition) model.draggableForms }


-- View

view : (Int, Int) -> Model -> Element
view (w, h) model =
  let
    forms = DF.render model.draggableForms
  in
    collage w h forms


---- Inputs ----

main : Signal Element
main =
  let
    model = Signal.foldp update initialState events
  in
    view <~ Window.dimensions ~ model


events : Signal Action
events = MouseMove <~ (relativePosition <~ Window.dimensions ~ pointerPosition)


pointerPosition : Signal (Int, Int)
pointerPosition =
  let goodTouches = Signal.filter (\list -> not (List.isEmpty list)) [] Touch.touches
      getTouchPosition = (\t -> (t.x, t.y))
  in
     Signal.merge
       ((List.map getTouchPosition >> List.head >> withDefault (0, 0)) <~ goodTouches)
       Mouse.position


relativePosition : (Int, Int) -> (Int, Int) -> (Int, Int)
relativePosition (w, h) (x, y) =  (x - w // 2, h // 2 - y)
