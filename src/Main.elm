module Main exposing (..)

import Html
import Model
import View


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init = Model.init
        , update = Model.update
        , subscriptions = Model.subscriptions
        , view = View.view
        }
