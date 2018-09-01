module Main exposing (..)

import Browser
import CreateFolderDialog as CreateFolderDialog exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, form, h1, i, input, li, text, textarea, ul)
import Html.Attributes exposing (class, disabled, hidden, href, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Utils exposing (onClickPreventDefault)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { items : Dict String FolderItem
    , path : List String
    , openedNote : Maybe String
    , dialog : Dialog
    }


type alias Note =
    { id : String
    , title : String
    , text : String
    }


type alias Folder =
    { id : String
    , title : String
    , items : List String
    }


type FolderItem
    = FolderItemNote Note
    | FolderItemFolder Folder


type Dialog
    = NoDialog
    | DialogCreateFolderDialog CreateFolderDialog.Model


init : Model
init =
    { items =
        Dict.fromList
            [ ( "0", FolderItemFolder (Folder "0" "Root" [ "1", "2", "3", "4" ]) )
            , ( "1", FolderItemNote (Note "1" "Note 1" "Note 1\n\nThis is a first note") )
            , ( "2", FolderItemNote (Note "2" "Note 2" "Note 2\n\nThis is a second note") )
            , ( "3", FolderItemNote (Note "3" "Note 3" "Note 3\n\nThis is a third note") )
            , ( "4", FolderItemFolder (Folder "4" "Folder 1" [ "5", "6", "7" ]) )
            , ( "5", FolderItemNote (Note "5" "Note A" "Note A\n\nThis is a note A") )
            , ( "6", FolderItemNote (Note "6" "Note B" "Note B\n\nThis is a note B") )
            , ( "7", FolderItemNote (Note "7" "Note C" "Note C\n\nThis is a note C") )
            ]
    , path = [ "0" ]
    , openedNote = Nothing
    , dialog = NoDialog
    }


getFolder : Dict String FolderItem -> List String -> Maybe Folder
getFolder items path =
    List.head path
        |> Maybe.andThen (\id -> Dict.get id items)
        |> Maybe.andThen
            (\item ->
                case item of
                    FolderItemNote _ ->
                        Nothing

                    FolderItemFolder f ->
                        Just f
            )


getFolderById : Dict String FolderItem -> String -> Maybe Folder
getFolderById items id =
    Dict.get id items
        |> Maybe.andThen
            (\item ->
                case item of
                    FolderItemNote _ ->
                        Nothing

                    FolderItemFolder f ->
                        Just f
            )


getNote : Dict String FolderItem -> String -> Maybe Note
getNote items id =
    Dict.get id items
        |> Maybe.andThen
            (\item ->
                case item of
                    FolderItemNote n ->
                        Just n

                    FolderItemFolder _ ->
                        Nothing
            )



-- UPDATE


type Msg
    = OpenFolder Folder
    | OpenNote Note
    | GoBack
    | ChangeNote String String
    | NewNote
    | OpenCreateFolderDialog
    | DialogMsg DialogMsg


type DialogMsg
    = DialogMsgCreateFolderDialog CreateFolderDialog.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        OpenFolder folder ->
            { model | path = folder.id :: model.path }

        OpenNote note ->
            { model | openedNote = Just note.id }

        GoBack ->
            model.path
                |> (\x ->
                        if List.length x > 1 then
                            Just x
                        else
                            Nothing
                   )
                |> Maybe.andThen (\x -> List.tail x)
                |> Maybe.map (\x -> { model | path = x })
                |> Maybe.withDefault model

        ChangeNote id text ->
            { model | items = Dict.insert id (FolderItemNote (Note id (genNoteTitle text) text)) model.items }

        NewNote ->
            let
                newId =
                    getNewId model.items
            in
            getFolder model.items model.path
                |> Maybe.map (\f -> { f | items = newId :: f.items })
                |> Maybe.map
                    (\f ->
                        { model
                            | openedNote = Just newId
                            , items =
                                model.items
                                    |> insertNote newId "New Note"
                                    |> insertFolder f
                        }
                    )
                |> Maybe.withDefault model

        DialogMsg dialogMsg ->
            case ( model.dialog, dialogMsg ) of
                ( DialogCreateFolderDialog dialogModel, DialogMsgCreateFolderDialog createFolderDialogMsg ) ->
                    case CreateFolderDialog.update createFolderDialogMsg dialogModel of
                        ( m, NoOp ) ->
                            { model | dialog = DialogCreateFolderDialog m }

                        ( _, CreateFolderDialog.Cancel ) ->
                            { model | dialog = NoDialog }

                        ( _, CreateFolderDialog.CreateFolder name ) ->
                            let
                                newId =
                                    getNewId model.items
                            in
                            getFolder model.items model.path
                                |> Maybe.map (\f -> { f | items = newId :: f.items })
                                |> Maybe.map
                                    (\f ->
                                        { model
                                            | dialog = NoDialog
                                            , items =
                                                model.items
                                                    |> insertFolder (Folder newId name [])
                                                    |> insertFolder f
                                        }
                                    )
                                |> Maybe.withDefault model

                ( _, _ ) ->
                    model

        OpenCreateFolderDialog ->
            let
                folderNames =
                    getFolder model.items model.path
                        |> Maybe.map (\f -> f.items |> List.filterMap (getFolderById model.items) |> List.map .title)
                        |> Maybe.withDefault []
            in
            { model | dialog = DialogCreateFolderDialog (CreateFolderDialog.initModel folderNames) }


genNoteTitle : String -> String
genNoteTitle text =
    text
        |> String.lines
        |> List.head
        |> Maybe.map String.trim
        |> Maybe.andThen
            (\x ->
                if String.length x == 0 then
                    Nothing
                else
                    Just x
            )
        |> Maybe.withDefault "untitled"


insertNote : String -> String -> Dict String FolderItem -> Dict String FolderItem
insertNote id text items =
    Dict.insert id (FolderItemNote (Note id (genNoteTitle text) text)) items


insertFolder : Folder -> Dict String FolderItem -> Dict String FolderItem
insertFolder folder items =
    Dict.insert folder.id (FolderItemFolder folder) items


getNewId : Dict String FolderItem -> String
getNewId items =
    items
        |> Dict.size
        |> (\x -> x + 1)
        |> String.fromInt



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewNotesList model
        , viewNoteEdit model
        , viewDialog model
        ]


viewNotesList : Model -> Html Msg
viewNotesList model =
    case getFolder model.items model.path of
        Just f ->
            viewFolder model.items f

        Nothing ->
            div [] [ text "Error: No Folder" ]


viewNoteEdit : Model -> Html Msg
viewNoteEdit model =
    model.openedNote
        |> Maybe.andThen (getNote model.items)
        |> viewNote


viewDialog : Model -> Html Msg
viewDialog model =
    case model.dialog of
        NoDialog ->
            text ""

        DialogCreateFolderDialog dialogModel ->
            Html.map (\x -> DialogMsg <| DialogMsgCreateFolderDialog x) (CreateFolderDialog.view dialogModel)


viewFolder : Dict String FolderItem -> Folder -> Html Msg
viewFolder items folder =
    div [ class "app__left-sidebar" ]
        [ div [ class "bar" ]
            [ button [ class "bar__button margin-right", onClick GoBack, hidden (folder.id == "0") ] [ i [ class "icon-angle-left" ] [] ]
            , div [ class "bar__title" ] [ text folder.title ]
            , button [ class "bar__button", onClick NewNote ] [ i [ class "icon-file" ] [] ]
            , button [ class "bar__button", onClick OpenCreateFolderDialog ] [ i [ class "icon-folder" ] [] ]
            ]
        , ul [ class "notes-list" ]
            (folder.items
                |> List.filterMap (\x -> Dict.get x items)
                |> List.map viewFolderItem
            )
        ]


viewFolderItem : FolderItem -> Html Msg
viewFolderItem folderItem =
    case folderItem of
        FolderItemNote note ->
            li [ class "notes-list__item" ]
                [ a [ class "notes-list__link", href "#", onClickPreventDefault (OpenNote note) ]
                    [ text note.title ]
                ]

        FolderItemFolder folder ->
            li [ class "notes-list__item notes-list__item--folder" ]
                [ a [ class "notes-list__link", href "#", onClickPreventDefault (OpenFolder folder) ]
                    [ text folder.title ]
                ]


viewNote : Maybe Note -> Html Msg
viewNote maybeNote =
    case maybeNote of
        Just note ->
            div [ class "app__main" ]
                [ div [ class "bar" ] [ div [ class "bar__title" ] [ text note.title ] ]
                , textarea [ class "note-editor", value note.text, onInput (ChangeNote note.id) ] []
                ]

        Nothing ->
            text ""
