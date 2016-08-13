module Tests.Stack exposing (..)

import Test exposing (Test, describe, test)
import Expect
import Stack exposing (isEmpty, Stack(..), cons, (:::), head, tail, (+++))


all : Test
all =
    describe "Stack"
        [ describe "isEmpty"
            [ test "returns true for empty stack"
                <| \() ->
                    isEmpty Nil
                        |> Expect.true "Expected isEmpty to return True for empty stack"
            , test "returns false for empty stack"
                <| \() ->
                    isEmpty (1 ::: Nil)
                        |> Expect.false "Expected isEmpty to return False for non-empty stack"
            ]
        , describe "cons (:::)"
            [ test "cons a number onto a stack"
                <| \() ->
                    (1 ::: Nil)
                        |> Expect.equal (Cons ( 1, Nil ))
            ]
        , describe "head"
            [ test "head will return the first element in a stack"
                <| \() ->
                    head (1 ::: Nil)
                        |> Expect.equal 1
            ]
        , describe "tail"
            [ test "tail will return the stack without the first element"
                <| \() ->
                    tail (1 ::: 2 ::: 3 ::: Nil)
                        |> Expect.equal (2 ::: 3 ::: Nil)
            ]
        , describe "concat (+++)"
            [ test "concat will return the stack without the first element"
                <| \() ->
                    ((1 ::: 2 ::: 3 ::: Nil) +++ (4 ::: 5 ::: Nil))
                        |> Expect.equal (1 ::: 2 ::: 3 ::: 4 ::: 5 ::: Nil)
            ]
        ]
