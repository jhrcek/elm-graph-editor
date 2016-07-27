port module Vis exposing (addNode, mkVisNode, addEdge, mkVisEdge, removeNode, removeEdge, nodeSelected, edgeSelected, unselectAll, EdgeId)

import Graph


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


port removeNode : Graph.NodeId -> Cmd msg


port addEdge : VisEdge -> Cmd msg


port removeEdge : EdgeId -> Cmd msg


port nodeSelected : (Graph.NodeId -> msg) -> Sub msg


port edgeSelected : (Int -> msg) -> Sub msg



-- () because ports MUST have at least one input arg


port unselectAll_ : () -> Cmd msg



-- declare helper to avoid having to call it with ()


unselectAll : Cmd msg
unselectAll =
    unselectAll_ ()
