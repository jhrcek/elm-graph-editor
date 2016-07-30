module Model exposing (init, update, subscriptions, Format(..), Gr, Model, Msg(..), NodeData, EdgeData)

import Platform.Cmd as Cmd
import Graph as G
import IntDict
import Vis exposing (..)
import Form exposing (Form, Msg(Submit))
import Form.Field as Field
import Form.Error exposing (Error(NotIncludedIn))
import Form.Validate exposing (Validation, get, oneOf, string, emptyString, int, form3, form5, customValidation)


type alias Gr =
    G.Graph NodeData EdgeData


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
    = NodeFormMsg Form.Msg
    | EdgeFormMsg Form.Msg
    | ChangeFormat Format
      -- Graph interaction
    | NodeSelected G.NodeId
    | EdgeSelected EdgeId


type alias Model =
    { graph : Gr
    , gens : IdGenerators
    , format : Format
    , nodeForm : Form () NodeData
    , edgeForm : Form () EdgeData
    , selectedNode : Maybe G.NodeId
    , selectedEdge : Maybe EdgeId
    }


type alias IdGenerators =
    { nodeUid : Int
    , edgeUid : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { graph = G.empty
      , gens = IdGenerators 0 0
      , format = ElmGraph
      , nodeForm = initNodeForm 0 "" ""
      , edgeForm = initEdgeForm 0 "" "" "" "" G.empty
      , selectedNode = Nothing
      , selectedEdge = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ graph, gens, format, nodeForm, edgeForm, selectedNode, selectedEdge } as model) =
    case msg of
        NodeFormMsg formMsg ->
            case ( formMsg, Form.getOutput nodeForm ) of
                ( Form.Focus "Add", Just newNode ) ->
                    cleanForms
                        { model
                            | graph = addNode newNode graph
                            , gens = { gens | nodeUid = gens.nodeUid + 1 }
                        }
                        ! [ Vis.addNode (Vis.mkVisNode newNode.nid newNode.label) ]

                ( Form.Focus "Remove", _ ) ->
                    case selectedNode of
                        Nothing ->
                            model ! []

                        Just nid ->
                            cleanForms { model | graph = G.remove nid graph }
                                ! [ Vis.removeNode nid ]

                ( Form.Focus "Update", Just updatedNode ) ->
                    case selectedNode of
                        Nothing ->
                            model ! []

                        Just nid ->
                            cleanForms { model | graph = updateNode updatedNode graph }
                                ! [ Vis.updateNode (Vis.mkVisNode updatedNode.nid updatedNode.label)
                                  , Vis.unselectAll
                                  ]

                ( Form.Focus "Unselect", _ ) ->
                    cleanForms model ! [ Vis.unselectAll ]

                _ ->
                    { model | nodeForm = Form.update formMsg nodeForm }
                        ! []

        EdgeFormMsg formMsg ->
            case ( formMsg, Form.getOutput edgeForm ) of
                ( Form.Focus "Add", Just newEdge ) ->
                    cleanForms
                        { model
                            | graph = addEdge newEdge graph
                            , gens = { gens | edgeUid = gens.edgeUid + 1 }
                        }
                        ! [ Vis.addEdge (Vis.mkVisEdge newEdge.eid newEdge.from newEdge.to newEdge.label) ]

                ( Form.Focus "Remove", _ ) ->
                    case selectedEdge of
                        Nothing ->
                            model ! []

                        Just eid ->
                            cleanForms { model | graph = removeEdge eid graph }
                                ! [ Vis.removeEdge eid ]

                ( Form.Focus "Update", Just updatedEdge ) ->
                    case selectedEdge of
                        Nothing ->
                            model ! []

                        Just eid ->
                            cleanForms
                                -- simply re-add the updated edge
                                { model | graph = addEdge updatedEdge graph }
                                ! [ Vis.updateEdge (Vis.mkVisEdge updatedEdge.eid updatedEdge.from updatedEdge.to updatedEdge.label)
                                  , Vis.unselectAll
                                  ]

                ( Form.Focus "Unselect", _ ) ->
                    cleanForms model ! [ Vis.unselectAll ]

                _ ->
                    { model | edgeForm = Form.update formMsg edgeForm }
                        ! []

        ChangeFormat fmt ->
            { model | format = fmt } ! []

        NodeSelected nid ->
            let
                retrieve nodeDataFieldAccessor =
                    Maybe.withDefault "" <| Maybe.map (nodeDataFieldAccessor << .label << .node) <| G.get nid model.graph
            in
                { model
                    | nodeForm = initNodeForm nid (retrieve .label) (retrieve .definition)
                    , edgeForm = initEdgeForm gens.edgeUid "" "" "" "" model.graph
                    , selectedNode = Just nid
                    , selectedEdge = Nothing
                }
                    ! []

        EdgeSelected eid ->
            let
                retrieveEdgeData accessor =
                    lookupEdge eid model.graph
                        |> Maybe.map accessor
                        |> Maybe.withDefault ""

                fromStr =
                    retrieveEdgeData (toString << .from)

                toStr =
                    retrieveEdgeData (toString << .to)

                label =
                    retrieveEdgeData (.label << .label)

                definition =
                    retrieveEdgeData (.definition << .label)
            in
                { model
                    | nodeForm = initNodeForm gens.nodeUid "" ""
                    , edgeForm = initEdgeForm eid fromStr toStr label definition model.graph
                    , selectedEdge = Just eid
                    , selectedNode = Nothing
                }
                    ! []


cleanForms : Model -> Model
cleanForms m =
    { m
        | nodeForm = initNodeForm m.gens.nodeUid "" ""
        , edgeForm = initEdgeForm m.gens.edgeUid "" "" "" "" m.graph
        , selectedNode = Nothing
        , selectedEdge = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Vis.nodeSelected NodeSelected
        , Vis.edgeSelected EdgeSelected
        ]



-- Graph helpers


addNode : NodeData -> Gr -> Gr
addNode ndata =
    let
        newNode =
            -- store all data as G.Node's label
            G.Node ndata.nid ndata
    in
        G.insert (G.NodeContext newNode IntDict.empty IntDict.empty)


updateNode : NodeData -> Gr -> Gr
updateNode ndata =
    let
        updatedNode =
            (G.Node ndata.nid ndata)
    in
        G.update ndata.nid (Maybe.map (\oldCtx -> { oldCtx | node = updatedNode }))


addEdge : EdgeData -> Gr -> Gr
addEdge ({ eid, from, to, label, definition } as edata) =
    let
        insertOutgoingEdge : G.NodeId -> EdgeData -> Maybe (G.NodeContext NodeData EdgeData) -> Maybe (G.NodeContext NodeData EdgeData)
        insertOutgoingEdge toId edgeData =
            Maybe.map
                (\oldCtx ->
                    { oldCtx
                        | outgoing = IntDict.insert toId edgeData oldCtx.outgoing
                    }
                )
    in
        G.update from (insertOutgoingEdge to edata)


lookupEdge : EdgeId -> Gr -> Maybe (G.Edge EdgeData)
lookupEdge eid gr =
    G.edges gr
        |> List.filter (\edge -> edge.label.eid == eid)
        |> List.head


removeEdge : EdgeId -> Gr -> Gr
removeEdge eid gr =
    let
        maybeEdge =
            lookupEdge eid gr

        removeOutgoingEdge : G.NodeId -> Maybe (G.NodeContext NodeData EdgeData) -> Maybe (G.NodeContext NodeData EdgeData)
        removeOutgoingEdge toId =
            Maybe.map
                (\oldCtx ->
                    { oldCtx
                        | outgoing = IntDict.remove toId oldCtx.outgoing
                    }
                )
    in
        case maybeEdge of
            Nothing ->
                gr

            Just edge ->
                G.update edge.from (removeOutgoingEdge edge.to) gr



-- Forms


initNodeForm : G.NodeId -> String -> String -> Form () NodeData
initNodeForm nid label definition =
    Form.initial
        [ ( "nid", Field.Text (toString nid) )
        , ( "label", Field.Text label )
        , ( "definition", Field.Text definition )
        ]
        validateAddNode


initEdgeForm : EdgeId -> String -> String -> String -> String -> Gr -> Form () EdgeData
initEdgeForm eid from to label definition graph =
    Form.initial
        [ ( "eid", Field.Text (toString eid) )
        , ( "from", Field.Text from )
        , ( "to", Field.Text to )
        , ( "label", Field.Text label )
        , ( "definition", Field.Text definition )
        ]
        (validateAddEdge graph)


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
