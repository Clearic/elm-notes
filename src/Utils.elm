module Utils exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Json


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    preventDefaultOn "click" (Json.map alwaysPreventDefault (Json.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )
