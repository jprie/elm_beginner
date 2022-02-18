module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, pre)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder, map2, field, string)
import Json.Encode as E

-- MAIN

-- defines which kind of app I get
main = Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


-- MODEL

-- defines the state
type Model
      = Failure Http.Error
      | Waiting
      | Success String
      | StudentSuccess Student

-- defines the first state

init : () -> (Model, Cmd Msg)
init _ =
        ( Waiting
        , Cmd.none
        )


-- UPDATE 

-- Msg defines the Events to be handled
type Msg 
        = Load
        | LoadStudent
        | GotHello (Result Http.Error String)
        | GotStudents (Result Http.Error Student)


-- Update defines how Msgs affect the state
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
        case msg of
               Load ->
                        (Waiting, getHello)
               LoadStudent ->
                        (Waiting, getStudents)
               GotHello result ->
                       case result of
                               Ok fullText ->
                                       (Success fullText, Cmd.none)
                               Err info ->
                                       (Failure info, Cmd.none)
               GotStudents result ->
                       case result of
                               Ok student ->
                                       (StudentSuccess student, Cmd.none)
                               Err info ->
                                       (Failure info, Cmd.none)
-- SUBSCRIPTION

subscriptions : Model -> Sub Msg
subscriptions model = 
        Sub.none

-- VIEW

-- defines how the state is showed in the UI
view : Model -> Html Msg
view model =
        case model of 
                Failure err ->
                        text ("Unable to load " ++ (errorToString err) )

                Waiting ->
                        div [] 
                          [ button [ onClick Load ] [ text "load hello" ]
                          , button [ onClick LoadStudent ] [ text "load student" ]
                          ]

                Success fullText ->
                        pre [] [ text fullText ]

                StudentSuccess student ->
                        pre [] [text (studentString student)]

studentString : Student -> String
studentString student = 
      student.firstName ++ " " ++
                        student.lastName 

-- HTTP

getHello : Cmd Msg
getHello = Http.get
                     { url = "http://localhost:8088/hello?name=Johannes"
                     , expect = Http.expectString GotHello 
                     }

type alias Student =
        { firstName : String
        , lastName : String
        }

getStudents : Cmd Msg
getStudents =
        Http.get
          { url = "http://localhost:8088/api/students/1"
          , expect = Http.expectJson GotStudents studentDecoder
          }

studentDecoder : Decoder Student
studentDecoder =
        map2 Student 
                (field "firstName" string)
                (field "lastName" string)


studentEncoder : Student -> E.Value
studentEncoder student =
        E.object
          [ ("firstName", E.string student.firstName)
          , ("lastName", E.string student.lastName)
          ]

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        NetworkError ->
            "Unable to reach the server, check your network connection"
        BadStatus 500 ->
            "The server had a problem, try again later"
        BadStatus 400 ->
            "Verify your information and try again"
        BadStatus _ ->
            "Unknown error"
        BadBody errorMessage ->
            errorMessage
