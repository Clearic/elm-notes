module CreateFolderDialog exposing (..)

import Html exposing (Html, button, div, form, i, input, text)
import Html.Attributes exposing (class, disabled, hidden, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


type alias Model =
    { name : String }


initModel =
    { name = "" }


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
            ( { model | name = name }, NoOp )

        Close ->
            ( model, Cancel )

        Submit ->
            ( model, CreateFolder model.name )


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
                        [ input [ class "popup__input", placeholder "Folder Name", value model.name, onInput ChangeName ] []
                        , button [ class "popup__button", disabled (String.length model.name == 0), type_ "submit" ] [ text "Create" ]
                        ]
                    ]
                ]
            ]
        ]
