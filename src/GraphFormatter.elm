module GraphFormatter exposing (formatGraph)

import Graph
import List
import String
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
toTgf g =
    let
        ns =
            Graph.nodes g
                |> List.map (\{ id, label } -> toString id ++ " " ++ label)

        es =
            Graph.edges g
                |> List.map (\{ from, to, label } -> toString from ++ " " ++ toString to ++ " " ++ label)
    in
        String.join "\n" <| ns ++ [ "#" ] ++ es


toElmGraph : Gr -> String
toElmGraph =
    Graph.toString'


toDot : Gr -> String
toDot _ =
    "TODO dot"
