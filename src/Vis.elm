port module Vis exposing (addNode, mkVisNode)


type alias VisNode =
    { id : Int
    , label : String
    }


mkVisNode : Int -> String -> VisNode
mkVisNode nId nLabel =
    { id = nId
    , label = nLabel
    }


port addNode : VisNode -> Cmd msg
