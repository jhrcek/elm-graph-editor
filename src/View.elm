module View exposing (view)

import Html exposing (..)
import Svg exposing (Svg, circle, g, text', tspan, polygon, path)
import Svg.Attributes exposing (width, height, viewBox, r, dx, cx, cy, x, y, stroke, fill, textAnchor, strokeWidth, style, transform, points, d)
import Model exposing (..)
import Maybe
import String


view : Model -> Html Msg
view m =
    div []
        [ div []
            [ h1 [] [ text "Graph editor" ]
            , div [] [ text "Start adding nodes by pressing 6" ]
            , stateAutomaton m.editState
            , editStatus m
            ]
        ]


editStatus : Model -> Html Msg
editStatus m =
    div []
        [ div [] [ text <| "Char buffer:" ++ m.charBuffer ]
        , div [ style "color:red;" ]
            [ m.invalidChar
                |> Maybe.map (\c -> "The character '" ++ String.fromChar c ++ "' is invalid in this state")
                |> Maybe.withDefault ""
                |> text
            ]
        ]

stateAutomaton : EditState -> Svg Msg
stateAutomaton st =
    Svg.svg [ width "350", height "360", viewBox "0 0 350 360" ]
        [ g [ transform "scale(0.715706 0.715706)" ]
            [ addEdgeCluster
            , automatonNode 40 279 "Start" (st == Start)
            , automatonNode 156 74 "Add Node" (st == AddNode)
            , automatonNode 156 218 "Set From" (st == SetFrom)
            , automatonNode 294 218 "Set To" (st == SetTo)
            , automatonNode 432 218 "Set Label" (st == SetLabel)
            , automatonNode 156 341 "Del Node" (st == DelNode)
            , automatonNode 156 464 "Del Edge" (st == DelEdge)
            , edge "M52,244 C63,209 83,154 112,113 C114,110 117,106 120,103"
                "118,101 127,97 122,106 117,101" 95 125 "6"
            , edge "M72,262 C85,255 100,246 114,239"
                "113,236 123,234 116,242 113,236" 98 242 "7"
            , edge "M72,296 C85,303 100,311 114,318"
                "116,315 123,323 112,322 116,315" 98 303 "2"
            , edge "M60,309 C79,341 109,390 130,424"
                "133,422 135,433 127,426 133,422" 98 361 "0"
            , edge "M146,38 C146,28 149,19 156,19 C160,19 164,23 165,28"
                "169,28 166,38 162,29 169,28" 156 15 "letters,spaces"
            , edge "M145,182 C145,172 148,163 155,163 C159,163 163,167 164,172"
                "168,172 165,182 161,173 168,172" 155 159 "numbers"
            , edge "M191,217 C208,217 228,217 247,217"
                "247,214 257,217 247,220 247,214" 224 213 "Enter"
            , edge "M280,183 C280,172 284,163 293,163 C299,163 303,167 304,173"
                "308,173 306,183 301,174 308,173" 293 159 "numbers"
            , edge "M329,217 C346,217 366,217 385,217"
                "385,214 395,217 385,220 385,214" 362 213 "Enter"
            , edge "M418,183 C418,172 422,163 431,163 C437,163 441,167 442,173"
                "446,173 444,183 439,174 446,173" 431 159 "letters, spaces"
            , edge "M145,305 C145,295 148,286 155,286 C159,286 163,290 164,295"
                "168,295 165,305 161,296 168,295" 155 282 "numbers"
            , edge "M145,428 C145,418 148,409 155,409 C159,409 163,413 164,418"
                "168,418 165,428 161,419 168,418" 155 405 "numbers"
            ]
        ]

addEdgeCluster : Svg a
addEdgeCluster =
    g []
        [ polygon [ fill "none", stroke "black", points "112,261 112,117 476,117 476,261 112,261" ] []
        , textLabel 293 132 "Add edge"
        ]

edge : String -> String -> Int -> Int -> String -> Svg a
edge pathData arrowheadPoints labelX labelY label =
    g []
        [ path [ fill "none", stroke "black", d pathData ] []
        , polygon [ fill "black", stroke "black", points arrowheadPoints ] []
        , textLabel labelX labelY label
        ]


automatonNode : Int -> Int -> String -> Bool -> Svg Msg
automatonNode cx_ cy_ label active =
    let
        r_ =
            36
    in
        g []
            [ circle
                [ cx (toString cx_)
                , cy (toString cy_)
                , r (toString r_)
                , stroke "black"
                , strokeWidth "1"
                , fill
                    (if active then
                        "lightblue"
                     else
                        "none"
                    )
                ]
                []
            , textLabel cx_ cy_ label
            ]

textLabel : Int -> Int -> String -> Svg c
textLabel x_ y_ label =
    text'
        [ x (toString x_)
        , y (toString y_)
        , textAnchor "middle"
        ]
        [ text label ]
