module Model exposing (init, update, Model, Msg(..), EditState(..))

import Graph as G
import IntDict
import Platform.Cmd as Cmd
import String exposing (fromChar)


type alias Gr =
    G.Graph String String


type alias Model =
    { graph : Gr
    , nextNodeId : Int
    , editState : EditState
    , labelBuffer : String
    , inputError : Maybe String
    }


type EditState
    = Start
    | AddNode
      -- The following 3 constitute adding edge
    | SetFrom
    | SetTo
    | SetLabel
      -- deleting
    | DelNode
    | DelEdge


type Msg
    = ChangeState EditState
    | AddChar Char
    | AddDigit Int
      -- Confirming input
    | ConfirmNodeLabel
    | ConfirmFrom
    | ConfirmTo
      -- Cancelling
    | ConfirmEdgeLabel
    | CancelEdit
      -- When we can't move to next state
    | InputError String

init : ( Model, Cmd a )
init =
    ( Model G.empty 0 Start "" Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        ChangeState st ->
            { model
                | editState = st
                , labelBuffer = ""
                , inputError = Nothing
            }

        AddChar chr ->
            { model
                | labelBuffer = model.labelBuffer ++ fromChar chr
                , inputError = Nothing
            }

        AddDigit dig ->
            { model | inputError = Nothing }

        ConfirmNodeLabel ->
            { model
                | graph = addNode model.nextNodeId model.labelBuffer model.graph
                , nextNodeId = model.nextNodeId + 1
                , editState = Start
                , labelBuffer = ""
                , inputError = Nothing
            }

        ConfirmFrom ->
            model

        ConfirmTo ->
            model

        ConfirmEdgeLabel ->
            { model | editState = Start, inputError = Nothing }

        CancelEdit ->
            { model
                | editState = Start
                , labelBuffer = ""
                , inputError = Nothing
            }

        InputError err ->
            { model | inputError = Just err, labelBuffer = "" }


addNode : G.NodeId -> String -> Gr -> Gr
addNode nid label =
    G.insert
        { node = G.Node nid label
        , incoming = IntDict.empty
        , outgoing = IntDict.empty
        }
