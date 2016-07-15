module View exposing (view)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events as E
import Dict
import Model exposing (..)
import GraphFormatter
import Model exposing (..)
import Form exposing (Form)
import Form.Input exposing (..)


view : Model -> Html Msg
view m =
    div []
        [ div []
            [ h1 [] [ text "Graph editor" ]
            , table []
                [ tr []
                    [ td [ class "bordered" ]
                        [ inputForm m ]
                    , td [ class "bordered", id "vis-container" ]
                        []
                    , td [ class "bordered", style [ ( "vertical-align", "top" ), ( "padding", "20px" ) ] ]
                        [ graphData m ]
                    ]
                ]
            ]
          --        , modelDebug m
        , graphEventsView m.graphEvents
        ]


inputForm : Model -> Html Msg
inputForm m =
    let
        selectedNodeControls =
            case m.selectedNode of
                Nothing ->
                    text ""

                Just nid ->
                    pureForm "Remove node"
                        [ pureControlGroup "selected node" (input [ type' "text", disabled True, size 3, value (toString nid) ] [])
                        , pureControls [ pureButton "Remove" RemoveNode ]
                        ]
    in
        div []
            [ Html.App.map AddingNode (addNodeFormView m.addNodeForm)
            , div [ style [ ( "color", "red" ) ] ] [ Html.App.map AddingNode <| dumpErrors m.addNodeForm ]
            , selectedNodeControls
            , Html.App.map AddingEdge (addEdgeFormView m.addEdgeForm)
            , div [ style [ ( "color", "red" ) ] ] [ Html.App.map AddingEdge <| dumpErrors m.addEdgeForm ]
            ]


graphData : Model -> Html Msg
graphData { format, graph } =
    div []
        [ h3 [] [ text "Graph data" ]
        , formatSelectionRadios
        , textarea
            [ value <| GraphFormatter.formatGraph format graph
            , rows 12
            , cols 50
            , readonly True
            ]
            []
        ]


formatSelectionRadios : Html Msg
formatSelectionRadios =
    let
        radio txt isChecked format =
            label []
                [ input [ type' "radio", name "format", checked isChecked, E.onCheck (\_ -> ChangeFormat format) ] []
                , text txt
                ]
    in
        div []
            [ text "Format: "
            , radio "elm-graph" True ElmGraph
            , radio "dot" False Dot
            , radio "tgf" False Tgf
            ]


modelDebug : Model -> Html Msg
modelDebug m =
    div [] [ text <| toString m ]


graphEventsView : GraphEvents -> Html Msg
graphEventsView events =
    div []
        [ h2 [] [ text "Graph events" ]
        , ul []
            <| List.map (\( id, event ) -> li [] [ text (toString id ++ " : " ++ toString event) ])
            <| Dict.toList events
        ]


addNodeFormView : Form () NodeData -> Html Form.Msg
addNodeFormView form =
    pureForm "Node"
        [ pureControlGroup "id"
            <| textInput (Form.getFieldAsString "nid" form) [ disabled True, size 3 ]
        , pureControlGroup "label"
            <| textInput (Form.getFieldAsString "label" form) [ placeholder "label" ]
        , pureControlGroup "definition"
            <| textArea (Form.getFieldAsString "definition" form) [ placeholder "definition" ]
        , div [ class "pure-controls" ]
            [ pureButton "Add" Form.Submit ]
        ]


addEdgeFormView : Form () EdgeData -> Html Form.Msg
addEdgeFormView form =
    pureForm "Edge"
        [ pureControlGroup "id"
            <| textInput (Form.getFieldAsString "eid" form) [ disabled True, size 3 ]
        , pureControlGroup "from"
            <| textInput (Form.getFieldAsString "from" form) [ size 3 ]
        , pureControlGroup "to"
            <| textInput (Form.getFieldAsString "to" form) [ size 3 ]
        , pureControlGroup "label"
            <| textInput (Form.getFieldAsString "label" form) [ placeholder "label" ]
        , pureControlGroup "definition"
            <| textArea (Form.getFieldAsString "definition" form) [ placeholder "definition" ]
        , pureControls [ pureButton "Add" Form.Submit ]
        ]



-- private Pure stuff


pureButton : String -> a -> Html a
pureButton label tagger =
    button [ class "pure-button pure-button-primary", E.onClick tagger ]
        [ text label ]


pureControlGroup : String -> Html a -> Html a
pureControlGroup fieldLabel fieldInput =
    div [ class "pure-control-group" ]
        [ label [] [ text fieldLabel ]
        , fieldInput
        ]


pureControls : List (Html a) -> Html a
pureControls =
    div [ class "pure-controls" ]


pureForm : String -> List (Html a) -> Html a
pureForm fieldsetLabel children =
    div [ class "pure-form pure-form-aligned" ]
        [ fieldset []
            ([ legend [] [ text fieldsetLabel ] ] ++ children)
        ]
