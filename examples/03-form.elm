module Main exposing (Model, Msg(..), init, main, update, validationPasswordLength, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    }


init : Model
init =
    Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if List.all (\f -> f model) [ validationPasswordMatch ] then
        if List.all (\f -> f model) [ validationComplexity, validationPasswordLength ] then
            div [ style "color" "green" ] [ text "OK" ]

        else
            div [ style "color" "black" ] [ text "OK, but not very secure" ]

    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]


validationPasswordMatch : Model -> Bool
validationPasswordMatch model =
    model.password == model.passwordAgain


validationPasswordLength : Model -> Bool
validationPasswordLength model =
    String.length model.password > 8


validationComplexity : Model -> Bool
validationComplexity model =
    List.all (\f -> f model.password) <| List.map String.any [ Char.isDigit, Char.isUpper, Char.isLower ]



-- This is a bit too complex compare to a using && for each, but I'm just practicing
