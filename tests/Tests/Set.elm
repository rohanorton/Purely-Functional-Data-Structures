module Tests.Set exposing (..)

import Test exposing (Test, describe, test, fuzz)
import Fuzz
import Expect
import My.Set
    exposing
        ( insert
        , empty
        , fromList
        , Set(..)
        , member
        , member'
        )


all : Test
all =
    describe "Set"
        [ describe "insert"
            [ test "inserts a number into an empty set"
                <| \() ->
                    empty
                        |> insert 1
                        |> Expect.equal (Node 1 Empty Empty)
            , test "inserts a number into a set with one element"
                <| \() ->
                    (Node 1 Empty Empty)
                        |> insert 2
                        |> Expect.equal (Node 1 Empty (Node 2 Empty Empty))
            , test "inserts a number into a set with one element"
                <| \() ->
                    (Node 2 Empty Empty)
                        |> insert 1
                        |> Expect.equal (Node 2 (Node 1 Empty Empty) Empty)
            , test "does not insert new node when element is duplicate"
                <| \() ->
                    (Node 1 Empty Empty)
                        |> insert 1
                        |> Expect.equal (Node 1 Empty Empty)
            ]
        , describe "member"
            [ test "returns false for empty set"
                <| \() ->
                    fromList []
                        |> member 1
                        |> Expect.false "Empty set does not contain 1"
            , test "returns true if elem is in set"
                <| \() ->
                    fromList [ 1, 2 ]
                        |> member 1
                        |> Expect.true "Set should contain 1"
            , test "returns true if elem is in non-empty set"
                <| \() ->
                    fromList [ 2, 3 ]
                        |> member 3
                        |> Expect.true "Set should contain 3"
            , test "returns false if elem is in non-empty set"
                <| \() ->
                    fromList [ 2, 3 ]
                        |> member 1
                        |> Expect.false "Set does not contain 1"
            ]
        , describe "member' -- two-way comparison implementation"
            [ fuzz (Fuzz.tuple ( Fuzz.list Fuzz.int, Fuzz.int )) "member' always returns true if element is in set"
                <| \( ints, elem ) ->
                    let
                        set =
                            fromList <| ints ++ [ elem ]
                    in
                        Expect.true "should always return true if elem is in set" (member' elem set)
            , fuzz (Fuzz.tuple ( Fuzz.list Fuzz.int, Fuzz.int )) "member and member' should produce same results"
                <| \( ints, elem ) ->
                    let
                        set =
                            fromList ints
                    in
                        Expect.equal (member elem set) (member' elem set)
            ]
        ]
