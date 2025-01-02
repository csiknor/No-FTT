module Utils exposing (..)

import CSS.Attributes exposing (classList)


classes =
    List.map (\a -> ( a, True )) >> classList
