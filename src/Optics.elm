module Main exposing (Lens, compose, composeLeft)


type alias Lens a b =
    { get : a -> b
    , set : b -> a -> a
    }


flip2 : (a -> b -> c) -> (b -> a -> c)
flip2 fn2 =
    \b -> \a -> fn2 a b


compose : Lens b c -> Lens a b -> Lens a c
compose bc ab =
    Lens
        (ab.get >> bc.get)
        (\item coll -> ab.set (bc.set item (ab.get coll)) coll)



-- composeLeft : Lens a b -> Lens b c -> Lens a c
-- composeLeft ab bc =
--     Lens
--         (ab.get >> bc.get)
--         (\item coll -> ab.set (bc.set item (ab.get coll)) coll)


composeLeft : Lens a b -> Lens b c -> Lens a c
composeLeft =
    flip2 compose
