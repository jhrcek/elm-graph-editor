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
          --, modelDebug m
        , graphEventsView m.graphEvents
        ]


inputForm : Model -> Html Msg
inputForm m =
    div []
        [ Html.App.map NodeFormMsg <| nodeFormView (m.selectedNode /= Nothing) m.nodeForm
        , Html.App.map EdgeFormMsg <| edgeFormView (m.selectedEdge /= Nothing) m.edgeForm
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


nodeFormView : Bool -> Form () NodeData -> Html Form.Msg
nodeFormView nodeSelected form =
    let
        nid =
            Form.getFieldAsString "nid" form

        label =
            Form.getFieldAsString "label" form

        definition =
            Form.getFieldAsString "definition" form
    in
        pureForm "Node"
            [ pureControlGroup nodeSelected "Selected node"
                <| textInput nid [ disabled True, size 3 ]
            , pureControlGroup True "label"
                <| textInput label []
            , pureControlGroup True "definition"
                <| textArea definition []
            , pureControls
                [ pureButton (not nodeSelected) "Add" (Form.Focus "Add")
                , pureButton nodeSelected "Remove" (Form.Focus "Remove")
                , pureButton nodeSelected "Update" (Form.Focus "Update")
                , pureButton nodeSelected "Unselect" (Form.Focus "Unselect")
                ]
            ]


edgeFormView : Bool -> Form () EdgeData -> Html Form.Msg
edgeFormView edgeElected form =
    let
        eid =
            Form.getFieldAsString "eid" form

        from =
            Form.getFieldAsString "from" form

        to =
            Form.getFieldAsString "to" form

        label =
            Form.getFieldAsString "label" form

        definition =
            Form.getFieldAsString "definition" form

        errorFor field =
            case field.liveError of
                Just error ->
                    "Node with this ID doesn't exist"

                Nothing ->
                    ""
    in
        pureForm "Edge"
            [ pureControlGroup edgeElected "Selected edge"
                <| textInput eid [ disabled True, size 3 ]
            , pureControlGroupWithError True
                "from"
                (textInput from [ size 3 ])
                (errorFor from)
            , pureControlGroupWithError True
                "to"
                (textInput to [ size 3 ])
                (errorFor to)
            , pureControlGroup True "label"
                <| textInput label []
            , pureControlGroup True "definition"
                <| textArea definition []
            , pureControls
                [ pureButton (not edgeElected) "Add" (Form.Focus "Add")
                , pureButton edgeElected "Remove" (Form.Focus "Remove")
                , pureButton edgeElected "Update" (Form.Focus "Update")
                , pureButton edgeElected "Unselect" (Form.Focus "Unselect")
                ]
            ]



-- private Pure stuff


pureButton : Bool -> String -> a -> Html a
pureButton visible label tagger =
    button
        [ visibility visible
        , class "pure-button"
        , E.onClick tagger
        ]
        [ text label ]


pureControlGroupWithError : Bool -> String -> Html a -> String -> Html a
pureControlGroupWithError visible fieldLabel fieldInput error =
    div
        [ visibility visible
        , class "pure-control-group"
        ]
        [ label [] [ text fieldLabel ]
        , fieldInput
        , span [ style [ ( "color", "red" ) ] ] [ text error ]
        ]


pureControlGroup : Bool -> String -> Html a -> Html a
pureControlGroup visible fieldLabel fieldInput =
    pureControlGroupWithError visible fieldLabel fieldInput ""


visibility : Bool -> Attribute msg
visibility visible =
    style
        [ if visible then
            ( "visibility", "visible" )
          else
            ( "display", "none" )
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
