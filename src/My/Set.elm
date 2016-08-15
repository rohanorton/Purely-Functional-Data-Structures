module My.Set exposing (..)

{-| Implementation of a set using a binary search tree
-}


type Set comparable
    = Empty
    | Node comparable (Set comparable) (Set comparable)


empty : Set comparable
empty =
    Empty


insert : comparable -> Set comparable -> Set comparable
insert elemToInsert set =
    case set of
        Empty ->
            Node elemToInsert Empty Empty

        Node nodeElem left right ->
            if (elemToInsert > nodeElem) then
                Node nodeElem left (insert elemToInsert right)
            else if (elemToInsert < nodeElem) then
                Node nodeElem (insert elemToInsert left) right
            else
                Node nodeElem left right


member : comparable -> Set comparable -> Bool
member elem set =
    case set of
        Empty ->
            False

        Node nodeElem left right ->
            if elem < nodeElem then
                member elem left
            else if elem > nodeElem then
                member elem right
            else
                True


{-| Two-way comparison search

    This implementation uses two-way comparison
    for a improved lookup speed

    http://user.it.uu.se/~arnea/ps/searchproc.pdf
-}
member' : comparable -> Set comparable -> Bool
member' elem set =
    twoWayComparison elem set Empty


twoWayComparison : comparable -> Set comparable -> Set comparable -> Bool
twoWayComparison elem node candidate =
    case node of
        Empty ->
            case candidate of
                Empty ->
                    False

                Node nodeElem _ _ ->
                    elem == nodeElem

        Node nodeElem left right ->
            if elem < nodeElem then
                twoWayComparison elem left candidate
            else
                twoWayComparison elem right node


{-| This was pretty satisfying to write :)
-}
fromList : List comparable -> Set comparable
fromList list =
    List.foldl insert empty list
