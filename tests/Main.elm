port module Main exposing (..)

import Test exposing (describe)
import Tests.Stack as Stack
import Tests.Set as Set
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Never
main =
    run emit
        <| describe "Purely functional data structures tests"
            [ Stack.all
            , Set.all
            ]


port emit : ( String, Value ) -> Cmd msg
