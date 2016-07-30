port module Vis
    exposing
        ( mkVisNode
        , addNode
        , updateNode
        , removeNode
        , mkVisEdge
        , addEdge
        , updateEdge
        , removeEdge
        , nodeSelected
        , edgeSelected
        , unselectAll
        , EdgeId
        )

import Graph


-- Graph library has concept of NodeId, but not EdgeId


type alias EdgeId =
    Int


type alias VisNode =
    { id : Int
    , label : String
    }


type alias VisEdge =
    { id : Int
    , from : Int
    , to : Int
    , label : String
    }


mkVisNode : Int -> String -> VisNode
mkVisNode =
    VisNode


mkVisEdge : Int -> Int -> Int -> String -> VisEdge
mkVisEdge =
    VisEdge


port addNode : VisNode -> Cmd msg


port updateNode : VisNode -> Cmd msg


port removeNode : Graph.NodeId -> Cmd msg


port addEdge : VisEdge -> Cmd msg


port updateEdge : VisEdge -> Cmd msg


port removeEdge : EdgeId -> Cmd msg


port nodeSelected : (Graph.NodeId -> msg) -> Sub msg


port edgeSelected : (Int -> msg) -> Sub msg


port unselectAll_ : () -> Cmd msg



-- () because ports MUST have at least one input arg
-- declare helper to avoid having to call it with ()


unselectAll : Cmd msg
unselectAll =
    unselectAll_ ()
