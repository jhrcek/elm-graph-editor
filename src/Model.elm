module Model exposing (init, update, subscriptions, GraphEvent(..), Format(..), Gr, GraphEvents, Model, Msg(..), NodeData, EdgeData)

import Platform.Cmd as Cmd
import Graph as G
import Dict as D
import IntDict
import Vis
import Form exposing (Form, Msg(Submit))
import Form.Field as Field
import Form.Error exposing (Error(NotIncludedIn))
import Form.Validate exposing (Validation, get, oneOf, string, emptyString, int, form3, form5, customValidation)


type alias Gr =
    G.Graph NodeData EdgeData


type alias GraphEvents =
    D.Dict Int GraphEvent


type alias EdgeId =
    Int



-- Graph library has concept of NodeId, but not EdgeId


type GraphEvent
    = AddNodeEvent NodeData
    | AddEdgeEvent EdgeData
    | RemoveNodeEvent Int
    | RemoveEdgeEvent Int


type alias NodeData =
    { nid : G.NodeId
    , label : String
    , definition : String
    }


type alias EdgeData =
    { eid : EdgeId
    , from : G.NodeId
    , to : G.NodeId
    , label : String
    , definition : String
    }


type Format
    = ElmGraph
    | Dot
    | Tgf


type Msg
    = AddingNode Form.Msg
    | AddingEdge Form.Msg
    | RemoveNode
    | ChangeFormat Format
      -- Graph interaction
    | NodeSelected G.NodeId
    | EdgeSelected Int


type alias Model =
    { graph : Gr
    , graphEvents : GraphEvents
    , gens : IdGenerators
    , format : Format
    , addNodeForm : Form () NodeData
    , addEdgeForm : Form () EdgeData
    , selectedNode : Maybe G.NodeId
    , selectedEdge : Maybe EdgeId
    }


type alias IdGenerators =
    { nodeUid : Int
    , edgeUid : Int
    , eventUid : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { graph = G.empty
      , graphEvents = D.empty
      , gens = IdGenerators 0 0 0
      , format = ElmGraph
      , addNodeForm = initAddNodeForm 0
      , addEdgeForm = initAddEdgeForm 0 G.empty
      , selectedNode = Nothing
      , selectedEdge = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ graph, graphEvents, gens, format, addNodeForm, addEdgeForm, selectedNode } as model) =
    case msg of
        AddingNode formMsg ->
            case ( formMsg, Form.getOutput addNodeForm ) of
                ( Form.Submit, Just newNode ) ->
                    let
                        newGraph =
                            addNode newNode graph
                    in
                        { model
                            | graph = newGraph
                            , addNodeForm = initAddNodeForm (gens.nodeUid + 1)
                            , addEdgeForm = initAddEdgeForm gens.edgeUid newGraph
                            , gens = IdGenerators (gens.nodeUid + 1) gens.edgeUid (gens.eventUid + 1)
                            , graphEvents = D.insert gens.eventUid (AddNodeEvent newNode) graphEvents
                        }
                            ! [ Vis.addNode (Vis.mkVisNode newNode.nid newNode.label) ]

                _ ->
                    { model | addNodeForm = Form.update formMsg addNodeForm }
                        ! []

        AddingEdge formMsg ->
            case ( formMsg, Form.getOutput addEdgeForm ) of
                ( Form.Submit, Just newEdge ) ->
                    let
                        newGraph =
                            addEdge newEdge graph

                        newGraphEvents =
                            D.insert gens.eventUid (AddEdgeEvent newEdge) graphEvents
                    in
                        { model
                            | graph = newGraph
                            , addEdgeForm = initAddEdgeForm (gens.edgeUid + 1) newGraph
                            , gens = IdGenerators gens.nodeUid (gens.edgeUid + 1) (gens.eventUid + 1)
                            , graphEvents = newGraphEvents
                        }
                            ! [ Vis.addEdge (Vis.mkVisEdge newEdge.eid newEdge.from newEdge.to newEdge.label) ]

                _ ->
                    { model | addEdgeForm = Form.update formMsg addEdgeForm }
                        ! []

        RemoveNode ->
            case selectedNode of
                Nothing ->
                    model ! []

                Just nid ->
                    let
                        newGraph =
                            G.remove nid graph

                        newGraphEvents =
                            D.insert gens.eventUid (RemoveNodeEvent nid) graphEvents
                    in
                        { model
                            | graph = newGraph
                            , gens = IdGenerators gens.nodeUid gens.edgeUid (gens.eventUid + 1)
                            , graphEvents = newGraphEvents
                            , selectedNode = Nothing
                        }
                            ! [ Vis.removeNode nid ]

        ChangeFormat fmt ->
            { model | format = fmt } ! []

        NodeSelected nid ->
            { model | selectedNode = Just nid, selectedEdge = Nothing } ! []

        EdgeSelected eid ->
            { model | selectedEdge = Just eid, selectedNode = Nothing } ! []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Vis.nodeSelected NodeSelected
        , Vis.edgeSelected EdgeSelected
        ]



-- Graph helpers


addNode : NodeData -> Gr -> Gr
addNode ({ nid, label, definition } as ndata) =
    G.insert
        { node = G.Node nid ndata
        , incoming = IntDict.empty
        , outgoing = IntDict.empty
        }


addEdge : EdgeData -> Gr -> Gr
addEdge ({ eid, from, to, label, definition } as edata) =
    G.update from (updateOutgoing to edata)


updateOutgoing : G.NodeId -> EdgeData -> Maybe (G.NodeContext NodeData EdgeData) -> Maybe (G.NodeContext NodeData EdgeData)
updateOutgoing toId edgeData maybeContext =
    maybeContext
        `Maybe.andThen` \{ incoming, node, outgoing } ->
                            Just
                                { node = node
                                , incoming = incoming
                                , outgoing = IntDict.insert toId edgeData outgoing
                                }



-- Forms


initAddNodeForm : Int -> Form () NodeData
initAddNodeForm initialNodeId =
    Form.initial [ ( "nid", Field.Text (toString initialNodeId) ) ] validateAddNode


initAddEdgeForm : Int -> Gr -> Form () EdgeData
initAddEdgeForm initialEdgeId graph =
    Form.initial [ ( "eid", Field.Text (toString initialEdgeId) ) ] (validateAddEdge graph)


validateAddNode : Validation () NodeData
validateAddNode =
    form3 NodeData
        (get "nid" int)
        (get "label" anyString)
        (get "definition" anyString)


validateAddEdge : Gr -> Validation () EdgeData
validateAddEdge graph =
    let
        nodeIdExistsInGraph =
            customValidation int
                (\id ->
                    if G.member id graph then
                        Ok id
                    else
                        --TODO custom error
                        Err NotIncludedIn
                )
    in
        form5 EdgeData
            (get "eid" int)
            (get "from" nodeIdExistsInGraph)
            (get "to" nodeIdExistsInGraph)
            (get "label" anyString)
            (get "definition" anyString)


anyString : Validation a String
anyString =
    oneOf [ emptyString, string ]
