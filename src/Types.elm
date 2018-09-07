module Types exposing (..)

import ContextMenu exposing (ContextMenu)
import Dict exposing (Dict)


type alias Note =
    { id : ID
    , parentId : ID
    , title : String
    , text : String
    }


type alias Folder =
    { id : ID
    , parentId : Maybe ID
    , title : String
    , items : List ID
    }


type alias ID =
    Int
