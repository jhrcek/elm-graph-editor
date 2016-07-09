port module Vis exposing (addNode, mkVisNode, addEdge, mkVisEdge, removeNode)

import Graph


type alias VisNode =
    { id : Int
    , label : String
    }


type alias VisEdge =
    { from : Int
    , to : Int
    , label : String
    }


mkVisNode : Int -> String -> VisNode
mkVisNode =
    VisNode


mkVisEdge : Int -> Int -> String -> VisEdge
mkVisEdge =
    VisEdge


port addNode : VisNode -> Cmd msg


port removeNode : Graph.NodeId -> Cmd msg


port addEdge : VisEdge -> Cmd msg
