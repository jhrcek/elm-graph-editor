port module Vis exposing (addNode, mkVisNode, addEdge, mkVisEdge, removeNode, nodeSelected, edgeSelected)

import Graph


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


port nodeSelected : (Graph.NodeId -> msg) -> Sub msg

port edgeSelected : (Int -> msg) -> Sub msg
