module Main exposing (Model, Msg(..), init, main, matchingExp, passwordEntropic, passwordsMatch, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , showMessage : Bool
    , validationMessage : String
    }


init : Model
init =
    { name = ""
    , password = ""
    , passwordAgain = ""
    , showMessage = False
    , validationMessage = ""
    }



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | ShowMessage
    | ValidationMessage String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        ShowMessage ->
            { model | showMessage = True }

        ValidationMessage message ->
            { model | validationMessage = message }



-- VIEW


view : Model -> Html Msg
view model =
    if model.showMessage then
        div []
            [ viewInput "text" "Name" model.name Name
            , viewInput "password" "Password" model.password Password
            , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
            , viewValidation model
            , button [] [ text "Submit" ]
            ]

    else
        div []
            [ viewInput "text" "Name" model.name Name
            , viewInput "password" "Password" model.password Password
            , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
            , button [ onClick ShowMessage ] [ text "Submit" ]
            ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if passwordsMatch model && passwordEntropic model then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text (generateMessage model) ]


passwordsMatch : Model -> Bool
passwordsMatch model =
    model.password == model.passwordAgain


matchingExp : Maybe Regex
matchingExp =
    Regex.fromString "^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[a-zA-Z]).{8,}$"


passwordEntropic : Model -> Bool
passwordEntropic model =
    case matchingExp of
        Nothing ->
            False

        Just val ->
            contains val model.password == True


generateMessage : Model -> String
generateMessage model =
    if passwordsMatch model /= True then
        "Passwords do not match!"

    else
        "Password must be at least 8 characters long and must include lowercase and uppercase letters and at least one number"
