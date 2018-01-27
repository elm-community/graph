module Tests.Graph.DOT exposing (all)

import Dict exposing (Dict)
import Expect
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Graph.DOT as DOT exposing (..)
import Test exposing (..)


all : Test
all =
    describe "DOT"
        [ describe "output" <|
            [ test "basic" <|
                let
                    nodes =
                        [ Node 0 "Welcome"
                        , Node 1 "To"
                        , Node 2 "Web"
                        , Node 3 "GraphViz!"
                        ]

                    e from to =
                        Edge from to ()

                    edges =
                        [ e 0 1
                        , e 1 2
                        , e 1 3
                        ]

                    g =
                        Graph.fromNodesAndEdges nodes edges

                    expected =
                        """digraph G {
  rankdir=TB;
  graph [];
  node [];
  edge [];

  0 -> 1;
  1 -> 2;
  1 -> 3;

  0 [label="Welcome"];
  1 [label="To"];
  2 [label="Web"];
  3 [label="GraphViz!"];
}"""

                    actual =
                        output Just (always Nothing) g
                in
                \() -> Expect.equal expected actual
            , test "with edge labels" <|
                let
                    nodes =
                        [ Node 0 "Welcome"
                        , Node 1 "To"
                        , Node 2 "Web"
                        , Node 3 "GraphViz!"
                        ]

                    e from to l =
                        Edge from to l

                    edges =
                        [ e 0 1 Nothing
                        , e 1 2 (Just "wait for it")
                        , e 1 3 (Just "ok")
                        ]

                    g =
                        Graph.fromNodesAndEdges nodes edges

                    expected =
                        """digraph G {
  rankdir=TB;
  graph [];
  node [];
  edge [];

  0 -> 1;
  1 -> 2 [label="wait for it"];
  1 -> 3 [label="ok"];

  0 [label="Welcome"];
  1 [label="To"];
  2 [label="Web"];
  3 [label="GraphViz!"];
}"""

                    actual =
                        output Just identity g
                in
                \() -> Expect.equal expected actual
            , test "with styles" <|
                let
                    nodes =
                        [ Node 0 { text = "Welcome" }
                        , Node 1 { text = "To" }
                        , Node 2 { text = "Web" }
                        , Node 3 { text = "GraphViz!" }
                        ]

                    e from to =
                        Edge from to ()

                    edges =
                        [ e 0 1
                        , e 1 2
                        , e 1 3
                        ]

                    g =
                        Graph.fromNodesAndEdges nodes edges

                    expected =
                        """digraph G {
  rankdir=LR;
  graph [bgcolor=red];
  node [shape=box, color=blue, style="rounded, filled"];
  edge [];

  0 -> 1;
  1 -> 2;
  1 -> 3;

  0 [label="Welcome"];
  1 [label="To"];
  2 [label="Web"];
  3 [label="GraphViz!"];
}"""

                    myStyles =
                        { defaultStyles
                            | rankdir = LR
                            , graph = "bgcolor=red"
                            , node = "shape=box, color=blue, style=\"rounded, filled\""
                        }

                    actual =
                        outputWithStyles myStyles (Just << .text) (always Nothing) g
                in
                \() -> Expect.equal expected actual
            , test "with styles with overrides" <|
                let
                    n id text style =
                        Node id { text = text, style = style }

                    nodes =
                        [ n 0 "Welcome" Nothing
                        , n 1 "To" Nothing
                        , n 2 "Web" Nothing
                        , n 3 "GraphViz!" (Just "bold,filled")
                        ]

                    e from to pw =
                        Edge from to { penwidth = pw }

                    edges =
                        [ e 0 1 Nothing
                        , e 1 2 Nothing 
                        , e 1 3 (Just 5)
                        ]

                    myStyles =
                        { defaultStyles
                            | node = "style=rounded"
                        }

                    g =
                        Graph.fromNodesAndEdges nodes edges

                    nodeAttrs n =
                        case n.style of
                            Nothing -> Dict.singleton "label" n.text
                            Just st -> Dict.fromList [("label", n.text), ("style", st)]

                    edgeAttrs e =
                        case e.penwidth of
                            Nothing -> Dict.empty
                            Just pw -> Dict.singleton "penwidth" (Basics.toString pw)

                    expected =
                        """digraph G {
  rankdir=TB;
  graph [];
  node [style=rounded];
  edge [];

  0 -> 1;
  1 -> 2;
  1 -> 3 [penwidth="5"];

  0 [label="Welcome"];
  1 [label="To"];
  2 [label="Web"];
  3 [label="GraphViz!", style="bold,filled"];
}"""

                    actual =
                        outputWithStylesAndAttributes myStyles nodeAttrs edgeAttrs g
                in
                \() -> Expect.equal expected actual
            ]
        ]
