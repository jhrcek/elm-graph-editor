module Main exposing (..)

import Html.App
import Model
import View


main : Program Never
main =
    Html.App.program
        { init = Model.init
        , update = Model.update
        , subscriptions = Model.subscriptions
        , view = View.view
        }
