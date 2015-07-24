module Shapes where

import Color exposing (green)
import DraggableForm as DF
import Graphics.Collage exposing (collage)
import Graphics.Element exposing (Element)
import Mouse
import Signal exposing ((<~), (~))
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
events = MouseMove <~ (relativePosition <~ Window.dimensions ~ Mouse.position)


relativePosition : (Int, Int) -> (Int, Int) -> (Int, Int)
relativePosition (w, h) (x, y) =  (x - w // 2, h // 2 - y)
