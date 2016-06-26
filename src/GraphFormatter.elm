module GraphFormatter exposing (formatGraph)

import Model exposing (..)

formatGraph : Format -> Gr -> String
formatGraph format graph =
    case format of
        ElmGraph ->
            toElmGraph graph

        Dot ->
            toDot graph

        Tgf ->
            toTgf graph


toTgf : Gr -> String
toTgf _ =
    "TODO tgf"


toElmGraph : Gr -> String
toElmGraph =
    toString


toDot : Gr -> String
toDot _ =
    "TODO dot"
