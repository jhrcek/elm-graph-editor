module Model exposing (init, update, Model, Msg(..), EditState(..))

import Graph as G
import Platform.Cmd as Cmd
import String exposing (fromChar)


type alias Gr =
    G.Graph (G.Node String) (G.Edge String)


type alias Model =
    { graph : Gr
    , nextNodeId : Int
    , nextEdgeId : Int
    , editState : EditState
    , charBuffer : String
    }


type EditState
    = Start
    | AddingNode
    | AddingEdge


type Msg
    = ChangeState EditState
    | AddChar Char


init =
    ( Model G.empty 0 0 Start "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeState st ->
            ( { model | editState = st }, Cmd.none )

        AddChar chr ->
            ( { model | charBuffer = model.charBuffer ++ fromChar chr }, Cmd.none )
