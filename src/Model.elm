module Model exposing (init, update, Model, Msg(..), Gr, EditState(..), Format(..))

import Graph as G
import IntDict
import Platform.Cmd as Cmd
import String exposing (fromChar)
import Vis


type alias Gr =
    G.Graph String String


type alias Model =
    { graph : Gr
    , nextNodeId : Int
    , editState : EditState
    , inputBuffer : String
    , nodeIdBuffer : List Int
    , inputError : Maybe String
    , format : Format
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


type Format
    = ElmGraph
    | Dot
    | Tgf


type Msg
    = ChangeFormat Format
    | ChangeState EditState
    | AddChar Char
      -- Confirming input
    | ConfirmNodeLabel
    | ConfirmFrom G.NodeId
    | ConfirmTo G.NodeId
      -- Cancelling
    | ConfirmEdgeLabel
    | CancelEdit
      -- Deleting
    | ConfirmNodeDeletion G.NodeId
      -- When we can't move to next state
    | InputError String


init : ( Model, Cmd a )
init =
    ( { graph = G.empty
      , nextNodeId = 0
      , editState = Start
      , inputBuffer = ""
      , nodeIdBuffer = []
      , inputError = Nothing
      , format = ElmGraph
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            updateModel msg model
    in
        case msg of
            ConfirmNodeLabel ->
                ( newModel, Vis.addNode (Vis.mkVisNode model.nextNodeId model.inputBuffer) )

            ConfirmEdgeLabel ->
                ( newModel, Vis.addEdge (Vis.mkVisEdge model.nodeIdBuffer model.inputBuffer) )

            ConfirmNodeDeletion nodeId ->
                ( newModel, Vis.removeNode nodeId )

            _ ->
                ( newModel, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        ChangeFormat fmt ->
            { model | format = fmt }

        ChangeState st ->
            { model
                | editState = st
                , inputBuffer = ""
                , inputError = Nothing
            }

        AddChar chr ->
            { model
                | inputBuffer = model.inputBuffer ++ fromChar chr
                , inputError = Nothing
            }

        ConfirmNodeLabel ->
            { model
                | graph = addNode model.nextNodeId model.inputBuffer model.graph
                , nextNodeId = model.nextNodeId + 1
                , editState = Start
                , inputBuffer = ""
                , inputError = Nothing
            }

        ConfirmFrom fromId ->
            { model
                | nodeIdBuffer = [ fromId ]
                , editState = SetTo
                , inputBuffer = ""
                , inputError = Nothing
            }

        ConfirmTo toId ->
            { model
                | nodeIdBuffer = model.nodeIdBuffer ++ [ toId ]
                , editState = SetLabel
                , inputBuffer = ""
                , inputError = Nothing
            }

        ConfirmEdgeLabel ->
            { model
                | graph = addEdge model.nodeIdBuffer model.inputBuffer model.graph
                , editState = Start
                , inputBuffer = ""
                , nodeIdBuffer = []
                , inputError = Nothing
            }

        CancelEdit ->
            { model
                | editState = Start
                , inputBuffer = ""
                , inputError = Nothing
            }

        InputError err ->
            { model
                | inputError = Just err
                , inputBuffer = ""
            }

        ConfirmNodeDeletion nodeId ->
            { model
                | graph = G.remove nodeId model.graph
                , editState = Start
                , inputError = Nothing
                , inputBuffer = ""
            }


addNode : G.NodeId -> String -> Gr -> Gr
addNode nid label =
    G.insert
        { node = G.Node nid label
        , incoming = IntDict.empty
        , outgoing = IntDict.empty
        }


addEdge : List G.NodeId -> String -> Gr -> Gr
addEdge nodeIds edgeLabel =
    case nodeIds of
        [ from, to ] ->
            G.update from (updateOutgoing to edgeLabel)

        unexpected ->
            Debug.crash <| "There should habe been 2 node IDs but there are " ++ toString unexpected


updateOutgoing : G.NodeId -> String -> Maybe (G.NodeContext String String) -> Maybe (G.NodeContext String String)
updateOutgoing toId edgeLabel maybeContext =
    case maybeContext of
        Nothing ->
            Nothing

        Just { incoming, node, outgoing } ->
            Just { node = node, incoming = incoming, outgoing = IntDict.insert toId edgeLabel outgoing }
