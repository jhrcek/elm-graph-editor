module Subscription exposing (keyboardSubscription)

import Char exposing (isUpper, isLower, isDigit)
import Graph
import Keyboard exposing (KeyCode)
import String
import Model exposing (..)


keyboardSubscription : Model -> Sub Msg
keyboardSubscription model =
    Keyboard.presses (transition model)


transition : Model -> KeyCode -> Msg
transition model code =
    let
        chr =
            Char.fromCode code
    in
        Debug.log "transition: "
            <| case ( model.editState, code ) of
                ( Start, 54 {- six -} ) ->
                    ChangeState AddNode

                ( Start, 55 {- seven -} ) ->
                    ChangeState SetFrom

                ( Start, 50 {- two -} ) ->
                    ChangeState DelNode

                ( Start, 48 {- zero -} ) ->
                    ChangeState DelEdge

                ( Start, code ) ->
                    invalidCharacter code chr

                ( AddNode, code ) ->
                    labelEditTransitions ConfirmNodeLabel code chr

                ( SetFrom, 13 {- Enter -} ) ->
                    if String.isEmpty model.labelBuffer then
                        InputError "You must enter ID of the edge's starting node"
                    else if not <| Graph.member (parseNodeId model.labelBuffer) model.graph then
                        InputError
                            <| "The node with ID "
                            ++ toString (parseNodeId model.labelBuffer)
                            ++ " does not exist in this graph. You must enter ID of one of the existing nodes"
                    else
                        ChangeState SetTo

                ( SetFrom, code ) ->
                    numberEditTransitions ConfirmFrom code chr

                ( SetTo, 13 {- Enter -} ) ->
                    ChangeState SetLabel

                ( SetTo, code ) ->
                    numberEditTransitions ConfirmTo code chr

                ( SetLabel, code ) ->
                    labelEditTransitions ConfirmEdgeLabel code chr

                ( DelNode, code ) ->
                    --TODO deletion confirmation
                    numberEditTransitions ConfirmTo code chr

                ( DelEdge, code ) ->
                    --TODO deletion confirmation
                    numberEditTransitions ConfirmTo code chr


parseNodeId : String -> Graph.NodeId
parseNodeId =
    Result.withDefault 0 << String.toInt


textEdittingTransitions : (Char -> Bool) -> Msg -> Int -> Char -> Msg
textEdittingTransitions acceptableChar confirmationMessage code chr =
    if acceptableChar chr then
        AddChar chr
    else if code == 13 {- Enter -} then
        confirmationMessage
    else if code == 96 {- ` -} || code == 126 {- ~ -} then
        CancelEdit
    else
        invalidCharacter code chr


labelEditTransitions : Msg -> Int -> Char -> Msg
labelEditTransitions =
    textEdittingTransitions (\c -> isUpper c || isLower c || isDigit c || c == ' ')


numberEditTransitions : Msg -> Int -> Char -> Msg
numberEditTransitions =
    textEdittingTransitions isDigit


invalidCharacter : Int -> Char -> Msg
invalidCharacter code chr =
    InputError <| "The character '" ++ String.fromChar chr ++ "' (ASCII code " ++ toString code ++ ") is invalid in this state"
