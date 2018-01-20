module Tests.Graph.TGF exposing (all)

import Expect
import Graph exposing (Edge, Node)
import Graph.TGF exposing (..)
import Test exposing (..)


all : Test
all =
    describe "TGF"
        [ describe "output" <|
            [ test "without edge labels" <|
                let
                    nodes =
                        [ Node 0 { text = "This" }
                        , Node 1 { text = "Is" }
                        , Node 2 { text = "TGF" }
                        , Node 3 { text = "Trivial" }
                        , Node 4 { text = "Graph" }
                        , Node 5 { text = "Format" }
                        ]

                    e from to =
                        Edge from to ()

                    edges =
                        [ e 0 1
                        , e 1 2
                        , e 3 2
                        , e 4 2
                        , e 5 2
                        ]

                    g =
                        Graph.fromNodesAndEdges nodes edges

                    expected =
                        """0 This
1 Is
2 TGF
3 Trivial
4 Graph
5 Format
#
0 1
1 2
3 2
4 2
5 2"""

                    actual =
                        output .text (always "") g
                in
                \() -> Expect.equal expected actual
            , test "with edge labels" <|
                let
                    nodes =
                        [ Node 0 { text = "This" }
                        , Node 1 { text = "Is" }
                        , Node 2 { text = "TGF" }
                        , Node 3 { text = "Trivial" }
                        , Node 4 { text = "Graph" }
                        , Node 5 { text = "Format" }
                        ]

                    e from to label =
                        Edge from to { text = label }

                    edges =
                        [ e 0 1 "a"
                        , e 1 2 "b"
                        , e 3 2 "c"
                        , e 4 2 "d"
                        , e 5 2 "e"
                        ]

                    g =
                        Graph.fromNodesAndEdges nodes edges

                    expected =
                        """0 This
1 Is
2 TGF
3 Trivial
4 Graph
5 Format
#
0 1 a
1 2 b
3 2 c
4 2 d
5 2 e"""

                    actual =
                        output .text .text g
                in
                \() -> Expect.equal expected actual
            ]
        ]
