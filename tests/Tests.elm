module Tests exposing (suite)

import Test exposing (..)
import Tests.Graph
import Tests.Graph.TGF
import Tests.Graph.Tree


suite : Test
suite =
    describe "elm-graph"
        [ Tests.Graph.all
        , Tests.Graph.Tree.all
        , Tests.Graph.TGF.all
        ]
