module Main exposing (..)

import Browser
import Browser.Dom as Dom
import ContextMenu exposing (ContextMenu)
import CreateFolderDialog as CreateFolderDialog exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, form, h1, i, input, li, text, textarea, ul)
import Html.Attributes exposing (class, disabled, hidden, href, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import RenameFolderDialog as RenameFolderDialog exposing (..)
import Task
import Types exposing (..)
import Utils exposing (filter, onClickPreventDefault)


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { items : Dict ID FolderItem
    , path : List ID
    , openedNote : Maybe ID
    , dialog : Dialog
    , contextMenu : ContextMenu ContextMenuContext
    }


type FolderItem
    = FolderItemNote Note
    | FolderItemFolder Folder


type Dialog
    = NoDialog
    | DialogCreateFolderDialog CreateFolderDialog.Model
    | DialogRenameFolderDialog RenameFolderDialog.Model


type ContextMenuContext
    = NoteContextMenu Note
    | FolderContextMenu Folder


contextMenuConfig : ContextMenu.Config
contextMenuConfig =
    { width = 300
    , direction = ContextMenu.RightBottom
    , overflowX = ContextMenu.Mirror
    , overflowY = ContextMenu.Mirror
    , containerColor = "white"
    , hoverColor = "rgb(240 240 240)"
    , invertText = False
    , cursor = ContextMenu.Pointer
    , rounded = False
    , fontFamily = "inherit"
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( contextMenu, msg ) =
            ContextMenu.init
    in
    ( { items =
            Dict.fromList
                [ ( "0", FolderItemFolder (Folder "0" Nothing "Root" [ "1", "2", "3", "4" ]) )
                , ( "1", FolderItemNote (Note "1" "0" "Note 1" "Note 1\n\nThis is a first note") )
                , ( "2", FolderItemNote (Note "2" "0" "Note 2" "Note 2\n\nThis is a second note") )
                , ( "3", FolderItemNote (Note "3" "0" "Note 3" "Note 3\n\nThis is a third note") )
                , ( "4", FolderItemFolder (Folder "4" (Just "0") "Folder 1" [ "5", "6", "7" ]) )
                , ( "5", FolderItemNote (Note "5" "4" "Note A" "Note A\n\nThis is a note A") )
                , ( "6", FolderItemNote (Note "6" "4" "Note B" "Note B\n\nThis is a note B") )
                , ( "7", FolderItemNote (Note "7" "4" "Note C" "Note C\n\nThis is a note C") )
                ]
      , path = [ "0" ]
      , openedNote = Nothing
      , dialog = NoDialog
      , contextMenu = contextMenu
      }
    , Cmd.map ContextMenuMsg msg
    )


newNote : ID -> ID -> String -> Note
newNote id parentId text =
    Note id parentId (genNoteTitle text) text


genNoteTitle : String -> String
genNoteTitle text =
    text
        |> String.lines
        |> List.head
        |> Maybe.map String.trim
        |> filter (\x -> String.length x /= 0)
        |> Maybe.withDefault "untitled"


insertNote : Note -> Dict ID FolderItem -> Dict ID FolderItem
insertNote note items =
    Dict.insert note.id (FolderItemNote note) items


insertFolder : Folder -> Dict ID FolderItem -> Dict ID FolderItem
insertFolder folder items =
    Dict.insert folder.id (FolderItemFolder folder) items


updateNote : ID -> String -> Dict ID FolderItem -> Dict ID FolderItem
updateNote id text items =
    getNote items id
        |> Maybe.map (\n -> { n | text = text, title = genNoteTitle text })
        |> Maybe.map (\n -> Dict.insert id (FolderItemNote n) items)
        |> Maybe.withDefault items


addItemToCurrentFolder : FolderItem -> List ID -> Dict ID FolderItem -> Maybe (Dict ID FolderItem)
addItemToCurrentFolder item path items =
    let
        id =
            getItemId item
    in
    getCurrentFolder items path
        |> Maybe.map
            (\f ->
                items
                    |> Dict.insert id item
                    |> insertFolder { f | items = id :: f.items }
            )


getItemId : FolderItem -> ID
getItemId item =
    case item of
        FolderItemNote note ->
            note.id

        FolderItemFolder folder ->
            folder.id


getCurrentFolder : Dict ID FolderItem -> List ID -> Maybe Folder
getCurrentFolder items path =
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


getFolder : Dict ID FolderItem -> ID -> Maybe Folder
getFolder items id =
    Dict.get id items
        |> Maybe.andThen
            (\item ->
                case item of
                    FolderItemNote _ ->
                        Nothing

                    FolderItemFolder f ->
                        Just f
            )


getNote : Dict ID FolderItem -> ID -> Maybe Note
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


removeItem : ID -> Dict ID FolderItem -> Dict ID FolderItem
removeItem id items =
    case Dict.get id items of
        Just (FolderItemNote note) ->
            Dict.remove note.id items

        Just (FolderItemFolder folder) ->
            removeFolder folder items

        Nothing ->
            items


removeFolder : Folder -> Dict ID FolderItem -> Dict ID FolderItem
removeFolder folder items =
    List.foldl removeItem items folder.items



-- UPDATE


type Msg
    = NoOp
    | OpenFolder Folder
    | NewNote
    | OpenNote Note
    | DeleteNote Note
    | ChangeNote ID String
    | GoBack
    | OpenCreateFolderDialog
    | CreateFolderDialogMsg CreateFolderDialog.Msg
    | OpenRenameFolderDialog Folder
    | RenameFolderDialogMsg RenameFolderDialog.Msg
    | DeleteFolder Folder
    | ContextMenuMsg (ContextMenu.Msg ContextMenuContext)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenFolder folder ->
            ( { model | path = folder.id :: model.path }, Cmd.none )

        OpenNote note ->
            ( { model | openedNote = Just note.id }, Cmd.none )

        GoBack ->
            model.path
                |> (\x ->
                        if List.length x > 1 then
                            List.tail x
                        else
                            Just x
                   )
                |> Maybe.map (\x -> ( { model | path = x }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        ChangeNote id text ->
            ( { model | items = updateNote id text model.items }, Cmd.none )

        NewNote ->
            let
                newId =
                    getNewId model.items
            in
            getCurrentFolder model.items model.path
                |> Maybe.map (\f -> newNote newId f.id "NewNote")
                |> Maybe.andThen (\note -> addItemToCurrentFolder (FolderItemNote note) model.path model.items)
                |> Maybe.map
                    (\items ->
                        ( { model
                            | openedNote = Just newId
                            , items = items
                          }
                        , focusNoteEditor
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        DeleteNote note ->
            getFolder model.items note.parentId
                |> Maybe.map (\f -> { f | items = List.filter (\x -> x /= note.id) f.items })
                |> Maybe.map (\f -> Dict.insert f.id (FolderItemFolder f) model.items)
                |> Maybe.map
                    (\items ->
                        ( { model
                            | items = Dict.remove note.id items
                            , openedNote = filter (\x -> x /= note.id) model.openedNote
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        DeleteFolder folder ->
            folder.parentId
                |> Maybe.andThen (\id -> getFolder model.items id)
                |> Maybe.map (\f -> { f | items = List.filter (\x -> x /= folder.id) f.items })
                |> Maybe.map (\f -> Dict.insert f.id (FolderItemFolder f) model.items)
                |> Maybe.map (\items -> ( { model | items = removeFolder folder items }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        OpenCreateFolderDialog ->
            let
                folderNames =
                    getCurrentFolder model.items model.path
                        |> Maybe.map (\f -> f.items |> List.filterMap (getFolder model.items) |> List.map .title)
                        |> Maybe.withDefault []

                ( dialogModel, cmd ) =
                    CreateFolderDialog.initModel folderNames
            in
            ( { model | dialog = DialogCreateFolderDialog dialogModel }, Cmd.map CreateFolderDialogMsg cmd )

        CreateFolderDialogMsg dialogMsg ->
            case model.dialog of
                DialogCreateFolderDialog dialogModel ->
                    case CreateFolderDialog.update dialogMsg dialogModel of
                        CreateFolderDialog.Update ( m, cmd ) ->
                            ( { model | dialog = DialogCreateFolderDialog m }, Cmd.map CreateFolderDialogMsg cmd )

                        CreateFolderDialog.CloseDialog ->
                            ( { model | dialog = NoDialog }, Cmd.none )

                        CreateFolderDialog.CreateFolder name ->
                            let
                                newId =
                                    getNewId model.items
                            in
                            getCurrentFolder model.items model.path
                                |> Maybe.map (\f -> Folder newId (Just f.id) name [])
                                |> Maybe.andThen (\folder -> addItemToCurrentFolder (FolderItemFolder folder) model.path model.items)
                                |> Maybe.map
                                    (\items ->
                                        ( { model
                                            | dialog = NoDialog
                                            , items = items
                                          }
                                        , Cmd.none
                                        )
                                    )
                                |> Maybe.withDefault ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OpenRenameFolderDialog folder ->
            let
                folderNames =
                    folder.parentId
                        |> Maybe.andThen (\id -> getFolder model.items id)
                        |> Maybe.map (\f -> f.items |> List.filterMap (getFolder model.items) |> List.map .title)
                        |> Maybe.withDefault []

                ( dialogModel, cmd ) =
                    RenameFolderDialog.initModel folder.id folder.title folderNames
            in
            ( { model | dialog = DialogRenameFolderDialog dialogModel }, Cmd.map RenameFolderDialogMsg cmd )

        NoOp ->
            ( model, Cmd.none )

        RenameFolderDialogMsg dialogMsg ->
            case model.dialog of
                DialogRenameFolderDialog dialogModel ->
                    case RenameFolderDialog.update dialogMsg dialogModel of
                        RenameFolderDialog.Update ( m, cmd ) ->
                            ( { model | dialog = DialogRenameFolderDialog m }, Cmd.map RenameFolderDialogMsg cmd )

                        RenameFolderDialog.CloseDialog ->
                            ( { model | dialog = NoDialog }, Cmd.none )

                        RenameFolderDialog.RenameFolder id name ->
                            getFolder model.items id
                                |> Maybe.map (\f -> { f | title = name })
                                |> Maybe.map
                                    (\f ->
                                        ( { model
                                            | dialog = NoDialog
                                            , items = insertFolder f model.items
                                          }
                                        , Cmd.none
                                        )
                                    )
                                |> Maybe.withDefault ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ContextMenuMsg msg_ ->
            let
                ( contextMenu, cmd ) =
                    ContextMenu.update msg_ model.contextMenu
            in
            ( { model | contextMenu = contextMenu }
            , Cmd.map ContextMenuMsg cmd
            )


getNewId : Dict ID FolderItem -> ID
getNewId items =
    items
        |> Dict.keys
        |> List.filterMap String.toInt
        |> List.maximum
        |> Maybe.map (\n -> String.fromInt (n + 1))
        |> Maybe.withDefault "0"


focusNoteEditor : Cmd Msg
focusNoteEditor =
    Task.attempt (\_ -> NoOp) (Dom.focus "note-editor")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ContextMenuMsg (ContextMenu.subscriptions model.contextMenu)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewNotesList model
        , viewNoteEdit model
        , viewDialog model
        , viewContextMenu model
        ]


viewNotesList : Model -> Html Msg
viewNotesList model =
    case getCurrentFolder model.items model.path of
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
            Html.map CreateFolderDialogMsg (CreateFolderDialog.view dialogModel)

        DialogRenameFolderDialog dialogModel ->
            Html.map RenameFolderDialogMsg (RenameFolderDialog.view dialogModel)


viewFolder : Dict ID FolderItem -> Folder -> Html Msg
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
                [ a [ class "notes-list__link", href "#", onClickPreventDefault (OpenNote note), ContextMenu.open ContextMenuMsg (NoteContextMenu note) ]
                    [ text note.title ]
                ]

        FolderItemFolder folder ->
            li [ class "notes-list__item notes-list__item--folder" ]
                [ a [ class "notes-list__link", href "#", onClickPreventDefault (OpenFolder folder), ContextMenu.open ContextMenuMsg (FolderContextMenu folder) ]
                    [ text folder.title ]
                ]


viewNote : Maybe Note -> Html Msg
viewNote maybeNote =
    case maybeNote of
        Just note ->
            div [ class "app__main" ]
                [ div [ class "bar" ] [ div [ class "bar__title" ] [ text note.title ] ]
                , textarea [ class "note-editor", id "note-editor", name "note-editor", value note.text, onInput (ChangeNote note.id) ] []
                ]

        Nothing ->
            text ""


viewContextMenu : Model -> Html Msg
viewContextMenu model =
    ContextMenu.view
        contextMenuConfig
        ContextMenuMsg
        toItemGroups
        model.contextMenu


toItemGroups : ContextMenuContext -> List (List ( ContextMenu.Item, Msg ))
toItemGroups context =
    case context of
        NoteContextMenu note ->
            [ [ ( ContextMenu.item "Open", OpenNote note )
              , ( ContextMenu.item "Delete", DeleteNote note )
              ]
            ]

        FolderContextMenu folder ->
            [ [ ( ContextMenu.item "Rename", OpenRenameFolderDialog folder )
              , ( ContextMenu.item "Delete", DeleteFolder folder )
              ]
            ]
