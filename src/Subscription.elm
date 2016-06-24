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

                ( AddNode, 13 ) ->
                    ConfirmNodeLabel

                ( AddNode, code ) ->
                    labelEditTransitions code chr

                ( SetFrom, 13 {- Enter -} ) ->
                    ensureNodeIdExists model.inputBuffer "the edge's starting node" model.graph (always ConfirmFrom)

                ( SetFrom, code ) ->
                    numberEditTransitions code chr

                ( SetTo, 13 {- Enter -} ) ->
                    ensureNodeIdExists model.inputBuffer "the edge's ending node" model.graph (always ConfirmTo)

                ( SetTo, code ) ->
                    numberEditTransitions code chr

                ( SetLabel, 13 ) ->
                    ConfirmEdgeLabel

                ( SetLabel, code ) ->
                    labelEditTransitions code chr

                ( DelNode, 13 {- Enter -} ) ->
                    ensureNodeIdExists model.inputBuffer "the edge's ending node" model.graph ConfirmNodeDeletion

                ( DelNode, code {- Enter -} ) ->
                    numberEditTransitions code chr

                ( DelEdge, code ) ->
                    --TODO deletion confirmation
                    numberEditTransitions code chr


textEdittingTransitions : (Char -> Bool) -> Int -> Char -> Msg
textEdittingTransitions acceptableChar code chr =
    if acceptableChar chr then
        AddChar chr
    else if code == 96 {- ` -} || code == 126 {- ~ -} then
        CancelEdit
    else
        invalidCharacter code chr


labelEditTransitions : Int -> Char -> Msg
labelEditTransitions =
    textEdittingTransitions (\c -> isUpper c || isLower c || isDigit c || c == ' ')


numberEditTransitions : Int -> Char -> Msg
numberEditTransitions =
    textEdittingTransitions isDigit


invalidCharacter : Int -> Char -> Msg
invalidCharacter code chr =
    InputError <| "The character '" ++ String.fromChar chr ++ "' (ASCII code " ++ toString code ++ ") is invalid in this state"


ensureNodeIdExists : String -> String -> Gr -> (Int -> Msg) -> Msg
ensureNodeIdExists potentialNodeId errorOnEmptyStr graph tagOnSuccess =
    let
        parsedId =
            parseNodeId potentialNodeId
    in
        if String.isEmpty potentialNodeId then
            InputError <| "You must enter ID of " ++ errorOnEmptyStr
        else if not <| Graph.member parsedId graph then
            InputError
                <| "The node with ID "
                ++ toString parsedId
                ++ " does not exist in this graph. You must enter ID of one of the existing nodes"
        else
            tagOnSuccess parsedId
