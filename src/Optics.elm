module Optics exposing (Lens, Prism, arrayIndex, compose, composeLeft)

import Array exposing (..)
import Dict exposing (..)



-- Lens


type alias Lens a b =
    { get : a -> b
    , set : b -> a -> a
    }


compose : Lens b c -> Lens a b -> Lens a c
compose bc ab =
    Lens
        (ab.get >> bc.get)
        (\item coll -> ab.set (bc.set item (ab.get coll)) coll)


composeLeft : Lens a b -> Lens b c -> Lens a c
composeLeft =
    flip2 compose



-- Prism


type alias Prism a b =
    { get : a -> Maybe b
    , set : b -> a -> a
    }


arrayIndex : Int -> Prism (Array a) a
arrayIndex index =
    { get = Array.get index, set = Array.set index }



-- is a dict prism useful at all?


dictKey : comparable -> Prism (Dict comparable v) v
dictKey key =
    { get = Dict.get key, set = Dict.insert key }



-- Util


flip2 : (a -> b -> c) -> (b -> a -> c)
flip2 fn2 =
    \b -> \a -> fn2 a b
