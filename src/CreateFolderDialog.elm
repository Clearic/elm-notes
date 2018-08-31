module CreateFolderDialog exposing (..)

import Html exposing (Html, button, div, form, i, input, text)
import Html.Attributes exposing (class, classList, disabled, hidden, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


type alias Model =
    { name : String
    , nameValMsg : Maybe String
    , namesAlreadyExist : List String
    }


initModel : List String -> Model
initModel names =
    { name = ""
    , nameValMsg = Nothing
    , namesAlreadyExist = List.map String.toLower names
    }


type Msg
    = ChangeName String
    | Close
    | Submit


type ExternalMsg
    = NoOp
    | CreateFolder String
    | Cancel


update : Msg -> Model -> ( Model, ExternalMsg )
update msg model =
    case msg of
        ChangeName name ->
            ( { model | name = name, nameValMsg = validateName model.namesAlreadyExist name }, NoOp )

        Close ->
            ( model, Cancel )

        Submit ->
            { model | nameValMsg = validateName model.namesAlreadyExist model.name }
                |> (\x ->
                        case x.nameValMsg of
                            Nothing ->
                                ( model, CreateFolder model.name )

                            Just _ ->
                                ( x, NoOp )
                   )


validateName : List String -> String -> Maybe String
validateName namesAlreadyExist name =
    name
        |> String.trim
        |> validateNameIsEmpty
        |> Result.andThen (validateNameIsAlreadyExist namesAlreadyExist)
        |> (\x ->
                case x of
                    Err e ->
                        Just e

                    Ok _ ->
                        Nothing
           )


validateNameIsEmpty : String -> Result String String
validateNameIsEmpty name =
    if String.length name == 0 then
        Err "Folder name must not be empty"
    else
        Ok name


validateNameIsAlreadyExist : List String -> String -> Result String String
validateNameIsAlreadyExist namesAlreadyExist name =
    if List.member (String.toLower name) namesAlreadyExist then
        Err "Folder with this name already exists"
    else
        Ok name


isModelValid : Model -> Bool
isModelValid model =
    case model.nameValMsg of
        Nothing ->
            True

        _ ->
            False


isNothing : Maybe a -> Bool
isNothing x =
    case x of
        Nothing ->
            True

        _ ->
            False


view : Model -> Html Msg
view model =
    div [ class "popup-back" ]
        [ div [ class "popup" ]
            [ div [ class "bar" ]
                [ div [ class "bar__title" ] [ text "Create Folder" ]
                , button [ class "bar__button", onClick Close ] [ i [ class "icon-close" ] [] ]
                ]
            , div [ class "popup__content" ]
                [ form [ onSubmit Submit ]
                    [ div [ class "form-row" ]
                        [ input
                            [ classList
                                [ ( "popup__input", True )
                                , ( "popup__input--invalid", not <| isNothing model.nameValMsg )
                                ]
                            , placeholder "Folder Name"
                            , value model.name
                            , onInput ChangeName
                            ]
                            []
                        , button [ class "popup__button", disabled (not <| isModelValid model), type_ "submit" ] [ text "Create" ]
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