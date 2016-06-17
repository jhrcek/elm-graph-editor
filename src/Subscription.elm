module Subscription exposing (keyboardSubscription)

import Model exposing (..)
import Keyboard exposing (KeyCode)
import Char


keyboardSubscription : Model -> Sub Msg
keyboardSubscription model =
    Keyboard.presses (toMessage model)


toMessage : Model -> KeyCode -> Msg
toMessage model code =
    case code of
        13 ->
            -- Enter
            ChangeState Start

        27 ->
            -- Escape
            ChangeState Start

        54 ->
            ChangeState AddingNode

        55 ->
            ChangeState AddingEdge

        c ->
            AddChar <| Char.fromCode c
