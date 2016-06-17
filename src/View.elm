module View exposing (view)

import Html exposing (..)
import Svg exposing (Svg, circle, g, text', tspan)
import Svg.Attributes exposing (width, height, viewBox, r, dx, cx, cy, x, y, stroke, fill, textAnchor, strokeWidth, style, transform)
import Model exposing (..)
import List


view : Model -> Html Msg
view m =
    div []
        [ Html.text "Hello here is some text"
        , stateAutomaton m.editState
        , text m.charBuffer
        ]


stateAutomaton : EditState -> Svg Msg
stateAutomaton st =
    Svg.svg [ width "600", height "600", style "border: solid 1px black" ]
        [ automatonNode 60 100 "Start" (st == Start)
        , automatonNode 200 60 "Adding node" (st == AddingNode)
        , automatonNode 200 180 "Adding edge" (st == AddingEdge)
        ]


automatonNode : Int -> Int -> String -> Bool -> Svg Msg
automatonNode nx ny label active =
    let
        radius =
            45
    in
        g []
            [ circle
                [ cx (toString nx)
                , cy (toString ny)
                , r (toString radius)
                , stroke
                    (if active then
                        "red"
                     else
                        "black"
                    )
                , strokeWidth "3px"
                , fill "white"
                ]
                []
            , text'
                [ x (toString <| nx)
                , y (toString <| ny)
                , textAnchor "middle"
                ]
                [ text label ]
            ]
