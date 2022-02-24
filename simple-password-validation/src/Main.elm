module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, input, div, text)
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
  { name = ""
  , password = ""
  , passwordAgain = ""
  }


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
    Password pw ->
      { model | password = pw }
    PasswordAgain pw2 ->
      { model | passwordAgain = pw2 }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Password again" model.passwordAgain PasswordAgain
    , viewValidation model
    ]
    
viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = 
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
  
viewValidation : Model -> Html msg
viewValidation model =
    passwordValidation model.password model.passwordAgain

-- password validation

passwordValidation : String -> String -> Html msg
passwordValidation password passwordAgain =
  if password /= passwordAgain then
    viewErrorMessage "Passwords do not match"
  else if containsNoNumerics password then
    viewErrorMessage "Password must contain numerics"
  else if containsNoLowerCase password then
    viewErrorMessage "Password does not contain lower case letters"
  else if containsNoUpperCase password then
    viewErrorMessage "Password does not contain upper case letters"
  else if String.length password < 8 then
    viewErrorMessage "Passwords must be at least 8 characters long"
  else
    div [ style "color" "green" ] [ text "Passwords do match" ]
  
viewErrorMessage str = 
    div [ style "color" "red" ] [ text str ]
    
-- if no lower case    
containsNoLowerCase : String -> Bool
containsNoLowerCase str =
    if String.toUpper str == str then
      True
    else
      False
      
containsNoUpperCase : String -> Bool
containsNoUpperCase str =
    if String.toLower str == str then
      True
    else
      False
      
containsNoNumerics : String -> Bool
containsNoNumerics str = 
  not (String.any Char.isDigit str)
