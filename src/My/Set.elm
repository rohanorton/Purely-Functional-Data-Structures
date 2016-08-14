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

        Node nodeElem lbranch rbranch ->
            if (elemToInsert > nodeElem) then
                Node nodeElem lbranch (insert elemToInsert rbranch)
            else if (elemToInsert < nodeElem) then
                Node nodeElem (insert elemToInsert lbranch) rbranch
            else
                Node nodeElem lbranch rbranch


member : comparable -> Set comparable -> Bool
member elem set =
    case set of
        Empty ->
            False

        Node nodeElem lbranch rbranch ->
            if elem < nodeElem then
                member elem lbranch
            else if elem > nodeElem then
                member elem rbranch
            else
                True


{-| This was pretty satisfying to write :)
-}
fromList : List comparable -> Set comparable
fromList list =
    List.foldl insert empty list
