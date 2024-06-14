module Graph.TGF exposing (output)

{-| This module provides a means of converting the `Graph` data type into a
valid [TGF](https://en.wikipedia.org/wiki/Trivial_Graph_Format) string for
visualizing your graph structure.

You can preview your graph by inserting the generated string into
[yEd](http://www.yworks.com/products/yed) or other compatible software.


# Conversion

@docs output

-}

import Graph exposing (Graph, edges, nodes)


{-| Converts a `Graph` into a valid TGF string.
-}
output : (node -> String) -> (edge -> String) -> Graph node edge -> String
output mapNode mapEdge graph =
    let
        nodes =
            Graph.nodes graph
                |> List.map
                    (\{ id, label } ->
                        String.fromInt id ++ " " ++ mapNode label
                    )
                |> List.sort

        edges =
            Graph.edges graph
                |> List.map
                    (\{ from, to, label } ->
                        String.fromInt from ++ " " ++ String.fromInt to ++ " " ++ mapEdge label
                    )
                |> List.sort
    in
    (nodes ++ "#" :: edges)
        -- trimming is questionable; little info about the format exists.
        -- yEd imports it fine though.
        |> List.map String.trim
        |> String.join "\n"
