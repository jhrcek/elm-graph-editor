module GraphFormatter exposing (formatGraph, getDefaultFileExtension)

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


getDefaultFileExtension : Format -> String
getDefaultFileExtension f =
    case f of
        ElmGraph ->
            "elmgraph"

        Dot ->
            "dot"

        Tgf ->
            "tgf"


toTgf : Gr -> String
toTgf g =
    let
        ns =
            Graph.nodes g
                |> List.map (\{ id, label } -> toString id ++ " " ++ label.label)

        es =
            Graph.edges g
                |> List.map (\{ from, to, label } -> toString from ++ " " ++ toString to ++ " " ++ label.label)
    in
        String.join "\n" <| ns ++ [ "#" ] ++ es


toElmGraph : Gr -> String
toElmGraph =
    Graph.toString'


toDot : Gr -> String
toDot g =
    let
        showLabel label =
            if String.isEmpty label then
                ";"
            else
                " [label=" ++ label ++ "];"

        ns =
            Graph.nodes g
                |> List.map (\{ id, label } -> String.padLeft 4 ' ' <| toString id ++ showLabel label.label)

        es =
            Graph.edges g
                |> List.map (\{ from, to, label } -> String.padLeft 4 ' ' <| toString from ++ " -> " ++ toString to ++ showLabel label.label)
    in
        String.join "\n" ("digraph {" :: ns ++ es ++ [ "}" ])
