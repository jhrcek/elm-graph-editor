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
                        [ Html.App.map AddingNode (addNodeFormView m.addNodeForm)
                        , div [ style [ ( "color", "red" ) ] ] [ Html.App.map AddingNode <| dumpErrors m.addNodeForm ]
                        , Html.App.map AddingEdge (addEdgeFormView m.addEdgeForm)
                        , div [ style [ ( "color", "red" ) ] ] [ Html.App.map AddingEdge <| dumpErrors m.addEdgeForm ]
                        ]
                    , td [ class "bordered", id "vis-container" ]
                        []
                    , td [ class "bordered", style [ ( "vertical-align", "top" ), ( "padding", "50px" ) ] ]
                        [ graphData m ]
                    ]
                ]
            ]
          --        , modelDebug m
        , graphEventsView m.graphEvents
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


addNodeFormView : Form () Node -> Html Form.Msg
addNodeFormView form =
    div [ class "pure-form pure-form-aligned" ]
        [ fieldset []
            [ legend [] [ text "Node" ]
            , controlGroup "id"
                <| textInput (Form.getFieldAsString "nid" form) [ disabled True, size 3 ]
            , controlGroup "label"
                <| textInput (Form.getFieldAsString "label" form) [ placeholder "label" ]
            , controlGroup "definition"
                <| textArea (Form.getFieldAsString "definition" form) [ placeholder "definition" ]
            , div [ class "pure-controls" ]
                [ button [ class "pure-button pure-button-primary", E.onClick Form.Submit ]
                    [ text "Add" ]
                ]
            ]
        ]


addEdgeFormView : Form () Edge -> Html Form.Msg
addEdgeFormView form =
    div [ class "pure-form pure-form-aligned" ]
        [ fieldset []
            [ legend [] [ text "Edge" ]
            , controlGroup "id"
                <| textInput (Form.getFieldAsString "eid" form) [ disabled True, size 3 ]
            , controlGroup "from"
                <| textInput (Form.getFieldAsString "from" form) [ size 3 ]
            , controlGroup "to"
                <| textInput (Form.getFieldAsString "to" form) [ size 3 ]
            , controlGroup "label"
                <| textInput (Form.getFieldAsString "label" form) [ placeholder "label" ]
            , controlGroup "definition"
                <| textArea (Form.getFieldAsString "definition" form) [ placeholder "definition" ]
            , div [ class "pure-controls" ]
                [ button [ class "pure-button pure-button-primary", E.onClick Form.Submit ]
                    [ text "Add" ]
                ]
            ]
        ]



-- private Pure stuff


controlGroup : String -> Html a -> Html a
controlGroup fieldLabel fieldInput =
    div [ class "pure-control-group" ]
        [ label [] [ text fieldLabel ]
        , fieldInput
        ]
