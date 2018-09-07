module RenameFolderDialog exposing (..)

import Browser.Dom as Dom
import Html exposing (Html, button, div, form, i, input, text)
import Html.Attributes exposing (class, classList, disabled, hidden, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Task
import Types exposing (ID)
import Utils exposing (isJust)


type alias Model =
    { id : ID
    , name : String
    , nameValMsg : Maybe String
    , namesAlreadyExist : List String
    }


initModel : ID -> String -> List String -> ( Model, Cmd Msg )
initModel id name names =
    ( { id = id
      , name = name
      , nameValMsg = Nothing
      , namesAlreadyExist = names |> List.filter (\x -> x /= name) |> List.map String.toLower
      }
    , focusFolderName
    )


focusFolderName : Cmd Msg
focusFolderName =
    Task.attempt (\_ -> NoOp) (Dom.focus "folder-name")


type Msg
    = NoOp
    | ChangeName String
    | Close
    | Submit


type ReturnModel
    = Update ( Model, Cmd Msg )
    | RenameFolder ID String
    | CloseDialog


update : Msg -> Model -> ReturnModel
update msg model =
    case msg of
        ChangeName name ->
            Update ( { model | name = name, nameValMsg = validateName model.namesAlreadyExist name }, Cmd.none )

        Close ->
            CloseDialog

        Submit ->
            { model | nameValMsg = validateName model.namesAlreadyExist model.name }
                |> (\x ->
                        case x.nameValMsg of
                            Nothing ->
                                RenameFolder model.id model.name

                            Just _ ->
                                Update ( x, Cmd.none )
                   )

        NoOp ->
            Update ( model, Cmd.none )


validateName : List String -> String -> Maybe String
validateName namesAlreadyExist name =
    let
        nameTrimmed =
            String.trim name

        nameAlreadyExists n =
            List.member (String.toLower n) namesAlreadyExist
    in
    if String.isEmpty nameTrimmed then
        Just "Folder name must not be empty"
    else if nameAlreadyExists nameTrimmed then
        Just "Folder with this name already exists"
    else
        Nothing


isModelValid : Model -> Bool
isModelValid model =
    case model.nameValMsg of
        Nothing ->
            True

        _ ->
            False


view : Model -> Html Msg
view model =
    div [ class "popup-back" ]
        [ div [ class "popup" ]
            [ div [ class "bar" ]
                [ div [ class "bar__title" ] [ text "Rename Folder" ]
                , button [ class "bar__button", onClick Close ] [ i [ class "icon-close" ] [] ]
                ]
            , div [ class "popup__content" ]
                [ form [ onSubmit Submit ]
                    [ div [ class "form-row" ]
                        [ input
                            [ classList
                                [ ( "popup__input", True )
                                , ( "popup__input--invalid", isJust model.nameValMsg )
                                ]
                            , id "folder-name"
                            , name "folder-name"
                            , placeholder "Folder Name"
                            , value model.name
                            , onInput ChangeName
                            ]
                            []
                        , button [ class "popup__button", disabled (not <| isModelValid model), type_ "submit" ] [ text "Save" ]
                        ]
                    , viewValMsg model.nameValMsg
                    ]
                ]
            ]
        ]


viewValMsg : Maybe String -> Html Msg
viewValMsg mayBeValMsg =
    case mayBeValMsg of
        Just valMsg ->
            div [ class "popup__validation-msg" ] [ text valMsg ]

        Nothing ->
            text ""
