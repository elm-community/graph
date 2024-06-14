module Tests.Graph.Tree exposing (all)

import Expect
import Graph.Tree as Tree exposing (Tree)
import Test exposing (..)


size : Tree a -> Int
size tree =
    tree
        |> Tree.preOrderList
        |> List.length


all : Test
all =
    let
        innerExample1 =
            Tree.inner 1 [ Tree.leaf 2, Tree.leaf 3, Tree.leaf 4 ]

        innerExample2 =
            Tree.inner 1 [ Tree.leaf 2, Tree.leaf 3, Tree.leaf 4, Tree.empty ]

        buildingTests =
            describe "building"
                [ test "empty has no nodes" <| \() -> Expect.equal 0 (size Tree.empty)
                , test "leaf has one node" <| \() -> Expect.equal 1 (size (Tree.leaf 42))
                , test "inner with 3 children has 3 nodes" <|
                    \() ->
                        Expect.equal 4 (size innerExample1)
                , test "inner removes empty children" <|
                    \() ->
                        Expect.equal innerExample1 innerExample2
                , test "unfoldTree" <|
                    \() ->
                        Expect.equal
                            innerExample1
                            (Tree.unfoldTree
                                (\s ->
                                    ( s
                                    , if s == 1 then
                                        [ 2, 3, 4 ]

                                      else
                                        []
                                    )
                                )
                                1
                            )
                ]

        queryTests =
            describe "query"
                [ test "empty isEmpty" <| \() -> Expect.equal True (Tree.isEmpty Tree.empty)
                , test "leaf is not empty" <| \() -> Expect.equal False (Tree.isEmpty (Tree.leaf 42))
                , test "inner with 2 children is not empty" <|
                    \() ->
                        Expect.equal False (Tree.isEmpty (Tree.leaf ()))
                , test "root of a non-empty tree" <|
                    \() ->
                        Expect.equal (Just ( 42, [] )) (Tree.root (Tree.leaf 42))
                , test "root of an empty tree" <|
                    \() ->
                        Expect.equal Nothing (Tree.root Tree.empty)
                , test "size of a non-empty tree" <|
                    \() ->
                        Expect.equal (Tree.size traversedTree) 7
                , test "height of a non-empty tree" <|
                    \() ->
                        Expect.equal (Tree.height traversedTree) 3
                , test "height of an empty tree" <|
                    \() ->
                        Expect.equal (Tree.height Tree.empty) 0
                ]

        mapTests =
            let
                sample =
                    Tree.inner 1 [ Tree.leaf 2, Tree.leaf 3, Tree.leaf 4, Tree.empty ]

                samplePlusOne =
                    Tree.inner 10 [ Tree.leaf 20, Tree.leaf 30, Tree.leaf 40, Tree.empty ]
            in
            describe "map"
                [ test "does not change empty" <|
                    \() ->
                        Expect.equal True (Tree.isEmpty <| Tree.map identity Tree.empty)
                            |> Expect.onFail "empty remains empty"
                , test "does not change structure of a tree" <|
                    \() -> Expect.equal sample <| Tree.map identity sample
                , test "applies function to all nodes" <|
                    \() -> Expect.equal samplePlusOne <| Tree.map (\x -> x * 10) sample
                ]

        traversedTree =
            Tree.inner 0
                [ Tree.inner 1
                    [ Tree.leaf 2, Tree.leaf 3 ]
                , Tree.inner 4
                    [ Tree.leaf 5, Tree.leaf 6 ]
                ]

        traversalTests =
            describe "traversal"
                [ test "levelOrderList" <|
                    \() ->
                        Expect.equal
                            [ 0, 1, 4, 2, 3, 5, 6 ]
                            (Tree.levelOrderList traversedTree)
                , test "postOrderList" <|
                    \() ->
                        Expect.equal
                            [ 2, 3, 1, 5, 6, 4, 0 ]
                            (Tree.postOrderList traversedTree)
                , test "preOrderList" <|
                    \() ->
                        Expect.equal
                            [ 0, 1, 2, 3, 4, 5, 6 ]
                            (Tree.preOrderList traversedTree)
                ]

        unitTests =
            describe "unit tests"
                [ buildingTests
                , queryTests
                , traversalTests
                , mapTests
                ]
    in
    describe "The Graph.Tree module"
        [ unitTests
        ]
