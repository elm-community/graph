module Tests.Graph exposing (all)

import Expect
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict exposing (IntDict)
import Test exposing (..)


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        _ ->
            False


isOk : Result m n -> Bool
isOk result =
    case result of
        Err _ ->
            False

        Ok _ ->
            True


isErr : Result m n -> Bool
isErr result =
    not (isOk result)


expectEqualComparing : (a -> b) -> a -> a -> Expect.Expectation
expectEqualComparing f a b =
    Expect.equal (f a) (f b)


edgeTriples : Graph n e -> List ( NodeId, NodeId, e )
edgeTriples =
    Graph.edges >> List.map (\e -> ( e.from, e.to, e.label ))


dressUp : Graph String ()
dressUp =
    let
        nodes =
            [ Node 0 "Socks"
            , Node 1 "Undershorts"
            , Node 2 "Pants"
            , Node 3 "Shoes"
            , Node 4 "Watch"
            , Node 5 "Shirt"
            , Node 6 "Belt"
            , Node 7 "Tie"
            , Node 8 "Jacket"
            ]

        e from to =
            Edge from to ()

        edges =
            [ e 0 3 -- socks before shoes
            , e 1 2 -- undershorts before pants
            , e 1 3 -- undershorts before shoes
            , e 2 3 -- pants before shoes
            , e 2 6 -- pants before belt
            , e 5 6 -- shirt before belt
            , e 5 7 -- shirt before tie
            , e 4 8 -- watch before jacket
            , e 6 8 -- belt before jacket
            , e 7 8 -- tie before jacket
            ]
    in
    Graph.fromNodesAndEdges nodes edges


dressUpWithCycle : Graph String ()
dressUpWithCycle =
    let
        nodes =
            [ Node 0 "Socks"
            , Node 1 "Undershorts"
            , Node 2 "Pants"
            ]

        e from to =
            Edge from to ()

        edges =
            [ e 0 1
            , e 1 2
            , e 2 0
            ]
    in
    Graph.fromNodesAndEdges nodes edges


connectedComponents : Graph Char ()
connectedComponents =
    let
        nodes =
            [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h' ]

        edges =
            [ ( 0, 1 )
            , ( 1, 2 )
            , ( 1, 4 )
            , ( 1, 5 )
            , ( 2, 3 )
            , ( 2, 6 )
            , ( 3, 2 )
            , ( 3, 7 )
            , ( 4, 0 )
            , ( 4, 5 )
            , ( 5, 6 )
            , ( 6, 5 )
            , ( 6, 7 )
            ]
    in
    Graph.fromNodeLabelsAndEdgePairs nodes edges


noNeighbors : Node String -> NodeContext String ()
noNeighbors node =
    NodeContext node IntDict.empty IntDict.empty


isValidTopologicalOrderingOf : Graph n e -> List (NodeContext n e) -> Bool
isValidTopologicalOrderingOf graph ordering =
    ordering
        |> List.foldl
            (\ctx maybeIds ->
                maybeIds
                    |> Maybe.andThen
                        (\ids ->
                            if List.all (\i ->IntDict.member i ids) (IntDict.keys ctx.incoming) then
                                ids |> IntDict.insert ctx.node.id () |> Just
                            else
                                Nothing
                        )
            )
            (Just IntDict.empty)
        |> isJust
        |> (&&) (List.length ordering == Graph.size graph)


expectTopologicalOrderingOf : Graph String e -> List (NodeContext String e) -> Expect.Expectation
expectTopologicalOrderingOf graph ordering =
    let
        message =
            String.join "\n"
                [ "Expected a valid topological ordering of "
                , "    " ++ Graph.toString identity (always "") graph
                , "but got"
                , "    " ++ Debug.toString ordering
                ]
    in
    Expect.true message (isValidTopologicalOrderingOf graph ordering)


all : Test
all =
    let
        emptyTests =
            describe "empty"
                [ test "has size 0" <| \() -> Expect.equal 0 (Graph.size Graph.empty)
                , test "isEmpty" <| \() -> Expect.equal True (Graph.isEmpty Graph.empty)
                ]

        memberTests =
            describe "member"
                [ test "True" <| \() -> Expect.equal True (Graph.member 0 dressUp)
                , test "False" <| \() -> Expect.equal False (Graph.member 99 dressUp)
                ]

        getTests =
            describe "get"
                [ test "id 0, the socks" <|
                    \() ->
                        Expect.equal
                            (Just "Socks")
                            (dressUp |> Graph.get 0 |> Maybe.map (.node >> .label))
                , test "id 99, Nothing" <| \() -> Expect.equal Nothing (Graph.get 99 dressUp)
                ]

        nodeIdRangeTests =
            describe "nodeIdRange"
                [ test "dressUp: [0, 8]" <|
                    \() ->
                        Expect.equal
                            (Just ( 0, 8 ))
                            (Graph.nodeIdRange dressUp)
                , test "dressUp - 0: [1, 8]" <|
                    \() ->
                        Expect.equal
                            (Just ( 1, 8 ))
                            (dressUp |> Graph.remove 0 |> Graph.nodeIdRange)
                , test "dressUp - 8: [0, 7]" <|
                    \() ->
                        Expect.equal
                            (Just ( 0, 7 ))
                            (dressUp |> Graph.remove 8 |> Graph.nodeIdRange)
                ]

        listRepTests =
            describe "list conversions"
                [ test "nodeIds" <|
                    \() ->
                        Expect.equal
                            [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
                            (dressUp |> Graph.nodeIds)
                , test "nodes" <|
                    \() ->
                        Expect.equal
                            [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
                            (dressUp |> Graph.nodes |> List.map .id)
                , test "edges" <|
                    \() ->
                        Expect.equal
                            [ ( 0, 3 ), ( 1, 2 ), ( 1, 3 ), ( 2, 3 ), ( 2, 6 ), ( 4, 8 ), ( 5, 6 ), ( 5, 7 ), ( 6, 8 ), ( 7, 8 ) ]
                            (dressUp
                                |> Graph.edges
                                |> List.map (\e -> ( e.from, e.to ))
                                |> List.sort
                            )
                ]

        insertTests =
            describe "insert"
                [ test "new node - size" <|
                    \() ->
                        Expect.equal
                            (dressUp |> Graph.size |> (+) 1)
                            (dressUp |> Graph.insert (noNeighbors (Node 99 "Ring")) |> Graph.size)
                , test "new node - can get it" <|
                    \() ->
                        Expect.equal
                            (Just "Ring")
                            (dressUp
                                |> Graph.insert (noNeighbors (Node 99 "Ring"))
                                |> Graph.get 99
                                |> Maybe.map (.node >> .label)
                            )
                , test "replace node - size" <|
                    \() ->
                        Expect.equal
                            (dressUp |> Graph.size)
                            (dressUp |> Graph.insert (noNeighbors (Node 0 "Ring")) |> Graph.size)
                , test "replace node - can get it" <|
                    \() ->
                        Expect.equal
                            (Just "Ring")
                            (dressUp
                                |> Graph.insert (noNeighbors (Node 0 "Ring"))
                                |> Graph.get 0
                                |> Maybe.map (.node >> .label)
                            )
                , test "replace node - replaces adjacency" <|
                    \() ->
                        Expect.equal
                            (Just True)
                            (dressUp
                                |> Graph.insert (noNeighbors (Node 0 "Ring"))
                                |> Graph.get 0
                                |> Maybe.map (\ctx -> IntDict.isEmpty ctx.incoming && IntDict.isEmpty ctx.outgoing)
                            )
                ]

        removeTests =
            describe "remove"
                [ test "nonexistent node" <|
                    \() ->
                        Expect.equal
                            dressUp
                            (dressUp |> Graph.remove 99)
                , test "existing node - size" <|
                    \() ->
                        Expect.equal
                            (dressUp |> Graph.size |> (\i -> (-) i 1))
                            (dressUp |> Graph.remove 0 |> Graph.size)
                , test "existing node - can't get it" <|
                    \() ->
                        Expect.equal
                            Nothing
                            (dressUp |> Graph.remove 0 |> Graph.get 0)
                ]

        updateTests =
            describe "update"
                [ test "remove outgoing edges" <|
                    \() ->
                        Expect.equal
                            (Just True)
                            (dressUp
                                |> Graph.update 0
                                    -- "Shorts" has outgoing edges
                                    (Maybe.map (\n -> { n | outgoing = IntDict.empty }))
                                |> Graph.get 0
                                |> Maybe.map (.outgoing >> IntDict.isEmpty)
                            )
                ]

        inducedSubgraphTests =
            describe "inducedSubgraph"
                [ test "should not have any dangling edges" <|
                    \() ->
                        expectEqualComparing
                            (edgeTriples >> List.sortBy (\( f, t, _ ) -> ( f, t )))
                            (Graph.fromNodesAndEdges
                                [ Node 0 'a', Node 1 'b', Node 4 'e' ]
                                [ Edge 0 1 (), Edge 1 4 (), Edge 4 0 () ]
                            )
                            (Graph.inducedSubgraph [ 0, 1, 4 ] connectedComponents)
                ]

        fromNodesAndEdgesTests =
            describe "fromNodesAndEdges"
                [ test "should not have any dangling edges" <|
                    \() ->
                        Expect.equal
                            [ Edge 0 0 () ]
                            (Graph.edges
                                (Graph.fromNodesAndEdges
                                    [ Node 0 'a' ]
                                    [ Edge 0 0 (), Edge 0 1 (), Edge 1 0 (), Edge 1 1 () ]
                                )
                            )
                ]

        foldTests =
            describe "fold"
                [ test "sum up ids" <|
                    \() ->
                        Expect.equal
                            36
                            (dressUp
                                |> Graph.fold (\ctx -> (+) ctx.node.id) 0
                            )
                ]

        mapTests =
            describe "map*"
                [ test "mapContexts over id is the id" <|
                    \() ->
                        Expect.equal
                            dressUp
                            (dressUp |> Graph.mapContexts identity)
                , test "mapNodes over id is the id" <|
                    \() ->
                        Expect.equal
                            dressUp
                            (dressUp |> Graph.mapNodes identity)
                , test "mapEdges over id is the id" <|
                    \() ->
                        Expect.equal
                            dressUp
                            (dressUp |> Graph.mapNodes identity)

                -- This should be backed by more tests, but I'm not in the mood for that :/
                ]

        graphOpsTests =
            describe "Graph ops"
                [ test "symmetricClosure is symmetric" <|
                    \() ->
                        Expect.true
                            "expected all incoming edges to also be outgoing and vice versa"
                            (dressUp
                                |> Graph.symmetricClosure (\_ _ e _ -> e)
                                |> Graph.fold
                                    (\ctx acc ->
                                        ctx.incoming == ctx.outgoing && acc
                                    )
                                    True
                            )
                , test "reverseEdges" <|
                    \() ->
                        Expect.equal
                            (dressUp
                                |> Graph.edges
                                |> List.map (\e -> ( e.from, e.to ))
                                |> List.sort
                            )
                            (dressUp
                                |> Graph.reverseEdges
                                |> Graph.edges
                                |> List.map (\e -> ( e.to, e.from ))
                                |> List.sort
                            )
                ]

        checkAcyclicTests =
            describe "checkAcyclicTests" <|
                [ test "Ok for graph with no cycles" <|
                    \() ->
                        Expect.true
                            "Should return Ok"
                            (isOk (Graph.checkAcyclic dressUp))
                , test "Err for cyclic graph" <|
                    \() ->
                        Expect.true
                            "Should return Err"
                            (isErr (Graph.checkAcyclic dressUpWithCycle))
                , test "Err for connectedComponents" <|
                    \() ->
                        Expect.true
                            "Should return Err"
                            (isErr (Graph.checkAcyclic connectedComponents))
                ]

        topologicalSortTests =
            describe "topologicalSort"
                [ test "valid topological ordering" <|
                    \() ->
                        case Graph.checkAcyclic dressUp of
                            Err e ->
                                Expect.fail
                                    ("dressUp should be acylic, but returned edge " ++ Debug.toString e)

                            Ok acyclic ->
                                acyclic
                                    |> Graph.topologicalSort
                                    |> expectTopologicalOrderingOf dressUp
                , test "heightLevels" <|
                    \() ->
                        case Graph.checkAcyclic dressUp of
                            Err e ->
                                Expect.fail
                                    ("dressUp should be acylic, but returned edge " ++ Debug.toString e)

                            Ok acyclic ->
                                acyclic
                                    |> Graph.heightLevels
                                    |> List.concat
                                    |> expectTopologicalOrderingOf dressUp
                ]

        bfsTests =
            describe "BFS"
                [ test "breadth-first node order" <|
                    \() ->
                        Expect.equal
                            [ 0, 3, 1, 2, 6, 8, 4, 5, 7 ]
                            (dressUp
                                |> Graph.bfs (Graph.ignorePath (::)) []
                                |> List.map (.node >> .id)
                                |> List.reverse
                            )
                ]

        graphWithLoop =
            Graph.fromNodeLabelsAndEdgePairs [ 0 ] [ ( 0, 0 ) ]

        sccTests =
            let
                result =
                    Graph.stronglyConnectedComponents connectedComponents

                sg nodeIds =
                    connectedComponents
                        |> Graph.inducedSubgraph nodeIds
                        |> Graph.toString String.fromChar (always "")
            in
            describe "Strongly connected components"
                [ test "The input graph was acyclic" <|
                    \() ->
                        Expect.true
                            "Result should be Err"
                            (isErr result)
                , test "The expected SCCs in order" <|
                    \() ->
                        Expect.equal
                            [ sg [ 0, 1, 4 ] -- "abe"
                            , sg [ 2, 3 ] -- "cd"
                            , sg [ 5, 6 ] -- "ef"
                            , sg [ 7 ] -- "h"
                            ]
                            (case result of
                                Err components ->
                                    List.map (Graph.toString String.fromChar (always "")) components

                                Ok _ ->
                                    []
                             -- should never happen oO
                            )
                , test "dressUp is acyclic" <|
                    \() ->
                        Expect.true
                            "Should be Ok"
                            (isOk (Graph.stronglyConnectedComponents dressUp))
                , test "The input graph has loops" <|
                    \() ->
                        Expect.true
                            "Should be Err"
                            (isErr (Graph.stronglyConnectedComponents graphWithLoop))
                ]

        unitTests =
            describe "unit tests"
                [ emptyTests
                , memberTests
                , getTests
                , nodeIdRangeTests
                , listRepTests
                , insertTests
                , removeTests
                , updateTests
                , inducedSubgraphTests
                , fromNodesAndEdgesTests
                , foldTests
                , mapTests
                , graphOpsTests
                , checkAcyclicTests
                , topologicalSortTests
                , bfsTests
                , sccTests
                ]

        examples =
            describe "examples"
                [ test "README - iWantToWearShoes" <|
                    \() ->
                        Expect.equal
                            [ "Pants", "Undershorts", "Socks", "Shoes" ]
                            iWantToWearShoes
                , test "insert" <|
                    \() ->
                        Expect.true "Graph size wasn't 2" insertExample
                , test "fold" <|
                    \() ->
                        Expect.true "The graph had a loop." foldExample
                , test "mapContexts" <|
                    \() ->
                        Expect.true "Mapped edge flip should've reversed edges" mapContextsExample
                ]
    in
    describe "The Graph module"
        [ unitTests
        , examples
        ]



-- EXAMPLE SECTION
-- The code of the more complex examples is exercised here
-- This is from the README


iWantToWearShoes : List String
iWantToWearShoes =
    Graph.guidedDfs
        Graph.alongIncomingEdges
        -- which edges to follow
        (Graph.onDiscovery
            (\ctx list ->
                -- append node labels on finish
                ctx.node.label :: list
            )
        )
        [ 3

        {- "Shoes" NodeId -}
        ]
        -- start with the node labelled "Shoes"
        []
        -- accumulate starting with the empty list
        dressUp
        -- traverse our dressUp graph from above
        |> Tuple.first



-- ignores the untraversed rest of the graph


insertExample : Bool
insertExample =
    let
        graph1 =
            Graph.fromNodesAndEdges [ Node 1 "1" ] []

        newNode =
            { node = Node 2 "2"
            , incoming = IntDict.singleton 1 () -- so there will be an edge from 1 to 2
            , outgoing = IntDict.empty
            }

        graph2 =
            Graph.insert newNode graph1
    in
    Graph.size graph2 == 2


foldExample : Bool
foldExample =
    let
        hasLoop ctx =
            IntDict.member ctx.node.id ctx.incoming

        graph =
            Graph.fromNodesAndEdges [ Node 1 "1", Node 2 "2" ] [ Edge 1 2 "->" ]

        -- The graph should not have any loop.
    in
    Graph.fold (\ctx acc -> acc || hasLoop ctx) False graph == False


mapContextsExample : Bool
mapContextsExample =
    let
        flipEdges ctx =
            { ctx | incoming = ctx.outgoing, outgoing = ctx.incoming }

        graph =
            Graph.fromNodesAndEdges [ Node 1 "1", Node 2 "2" ] [ Edge 1 2 "->" ]
    in
    Graph.reverseEdges graph == Graph.mapContexts flipEdges graph


symmetricClosureExample : Bool
symmetricClosureExample =
    let
        graph =
            Graph.fromNodesAndEdges [ Node 1 "1", Node 2 "2" ] [ Edge 1 2 "->" ]

        onlyUndirectedEdges ctx =
            ctx.incoming == ctx.outgoing

        merger from to outgoingLabel incomingLabel =
            outgoingLabel

        -- quite arbitrary, will not be called for the above graph
    in
    Graph.fold
        (\ctx acc -> acc && onlyUndirectedEdges ctx)
        True
        (Graph.symmetricClosure merger graph)
        == True


onDiscoveryExample : ()



-- Just let it compile


onDiscoveryExample =
    let
        dfsPostOrder : Graph n e -> List (NodeContext n e)
        dfsPostOrder graph =
            Graph.dfs (Graph.onDiscovery (::)) [] graph
    in
    dfsPostOrder Graph.empty |> (\_ -> ())


onFinishExample : ()



-- Just let it compile


onFinishExample =
    let
        dfsPreOrder : Graph n e -> List (NodeContext n e)
        dfsPreOrder graph =
            Graph.dfs (Graph.onFinish (::)) [] graph
    in
    dfsPreOrder Graph.empty |> (\_ -> ())


ignorePathExample : ()



-- Just let it compile


ignorePathExample =
    let
        bfsLevelOrder : Graph n e -> List (NodeContext n e)
        bfsLevelOrder graph =
            graph
                |> Graph.bfs (Graph.ignorePath (::)) []
                |> List.reverse
    in
    bfsLevelOrder Graph.empty |> (\_ -> ())
