port module Vis exposing (addNode, mkVisNode, addEdge, mkVisEdge)


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
mkVisNode nId nLabel =
    { id = nId
    , label = nLabel
    }


mkVisEdge : List Int -> String -> VisEdge
mkVisEdge ids label =
    case ids of
        [ from, to ] ->
            { from = from, to = to, label = label }

        unexpected ->
            Debug.crash <| "There should habe been 2 node IDs but there are " ++ toString unexpected


port addNode : VisNode -> Cmd msg


port addEdge : VisEdge -> Cmd msg
