module Main exposing (..)

import Html.App
import Model
import View
import Platform.Sub as Sub

main : Program Never
main =
    Html.App.program
        { init = Model.init
        , update = Model.update
        , subscriptions = always Sub.none
        , view = View.view
        }
