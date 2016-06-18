module Subscription exposing (keyboardSubscription)

import Model exposing (..)
import Keyboard exposing (KeyCode)
import Char exposing (isUpper, isLower, isDigit)


keyboardSubscription : Model -> Sub Msg
keyboardSubscription model =
    Keyboard.presses (transition model.editState)


transition : EditState -> KeyCode -> Msg
transition state code =
    let
        chr =
            Char.fromCode code
    in
        case ( state, code ) of
            ( Start, 54 {- six -} ) ->
                ChangeState AddNode

            ( Start, 55 {- seven -} ) ->
                ChangeState SetFrom

            ( Start, 50 {- two -} ) ->
                ChangeState DelNode

            ( Start, 48 {- zero -} ) ->
                ChangeState DelEdge

            ( Start, invalidCode ) ->
                InvalidChar chr

            ( AddNode, code ) ->
                labelEditTransitions code chr

            ( SetFrom, code ) ->
                numberEditTransitions code chr

            ( SetTo, code ) ->
                numberEditTransitions code chr

            ( SetLabel, code ) ->
                labelEditTransitions code chr

            ( DelNode, code ) ->
                numberEditTransitions code chr

            ( DelEdge, code ) ->
                numberEditTransitions code chr


textEdittingTransitions : (Char -> Bool) -> Int -> Char -> Msg
textEdittingTransitions acceptableChar code chr =
    Debug.log ""
        <| if acceptableChar chr then
            AddChar chr
           else if code == 13 {- Enter -} then
            ConfirmEdit
           else if code == 96 {- ` -} || code == 126 {- ~ -} then
            CancelEdit
           else
            InvalidChar chr


labelEditTransitions : Int -> Char -> Msg
labelEditTransitions =
    textEdittingTransitions (\c -> isUpper c || isLower c || isDigit c || c == ' ')


numberEditTransitions : Int -> Char -> Msg
numberEditTransitions =
    textEdittingTransitions isDigit
