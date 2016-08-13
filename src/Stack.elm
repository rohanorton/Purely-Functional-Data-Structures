module Stack exposing (..)

{-| Implementation of a persistant stack
-}


type Stack a
    = Nil
    | Cons ( a, Stack a )


isEmpty : Stack a -> Bool
isEmpty stack =
    stack == Nil


cons : a -> Stack a -> Stack a
cons x stack =
    Cons ( x, stack )


(:::) : a -> Stack a -> Stack a
(:::) =
    cons


head : Stack a -> a
head stack =
    case stack of
        Nil ->
            Debug.crash "Cannot get head of empty stack"

        Cons ( x, xs ) ->
            x


tail : Stack a -> Stack a
tail stack =
    case stack of
        Nil ->
            Debug.crash "Cannot get tail of empty stack"

        Cons ( x, xs ) ->
            xs


concat : Stack a -> Stack a -> Stack a
concat stackA stackB =
    case (stackA) of
        Nil ->
            stackB

        Cons ( x, xs ) ->
            x ::: concat xs stackB


(+++) : Stack a -> Stack a -> Stack a
(+++) =
    concat


update : Stack a -> Int -> a -> Stack a
update stack index y =
    case ( stack, index ) of
        ( Cons ( x, xs ), 0 ) ->
            y ::: xs

        ( Cons ( x, xs ), i ) ->
            x ::: update xs (i - 1) y

        _ ->
            Debug.crash "Could not update value in list"


suffixes : Stack a -> Stack (Stack a)
suffixes stack =
    case stack of
        Cons ( x, xs ) ->
            Cons ( stack, suffixes xs )

        Nil ->
            Nil
