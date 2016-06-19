module Main exposing (..)

import Html.App
import Model
import View
import Subscription

main : Program Never
main =
    Html.App.program
        { init = Model.init
        , update = Model.update
        , subscriptions = Subscription.keyboardSubscription
        , view = View.view
        }
