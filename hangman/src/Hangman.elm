module Hangman exposing (..)

import Browser
import Html exposing (Html, button, div, text, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Encode exposing (string)
import VirtualDom
import Http
import Set exposing (Set)


-- MAIN

main = 
   Browser.element 
   { init = init 
   , update = update
   , subscriptions = subscriptions
   , view = view
   }


-- MODEL

type alias Model
  = { guessWord : String
    , guessedLetters : (Set String)
    }

init : () -> ( Model, Cmd Msg )
init _ = 
        (Model "Hello World" Set.empty, getWordFromServer)

getWordFromServer : Cmd Msg
getWordFromServer = 
  Http.get 
    { url = "http://localhost:8088/word"
    , expect = Http.expectString GotGuessWord
    }

-- UPDATE

type Msg = Guess String
        | GotGuessWord (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
        case msg of
                Guess char ->
                        (updatedModel model char, Cmd.none)
                GotGuessWord result -> 
                        case result of
                                Ok word ->
                                        ({ model | guessWord = word }, Cmd.none)
                                Err _ ->
                                        (model, Cmd.none)

updatedModel : Model -> String -> Model
updatedModel model char =
        let updatedGuessedLetters = Set.insert (String.toLower char) model.guessedLetters
        in
        { model | guessedLetters = updatedGuessedLetters }

-- SUBSCRIPTION

subscriptions : Model -> Sub Msg
subscriptions model =
        Sub.none


-- VIEW

view : Model -> Html Msg
view model = 
        div []
        [stylesheetLink "styles.css"
        , div [ class "welcome" ] [ text welcome ]
        , div [ class "text-field" ] [text (guessFormatter model) ]
        , buttons
        , winnerMessage model
        ]

winnerMessage : Model -> Html Msg
winnerMessage model =
        if String.contains "_" (guessFormatter model) then
          div [ class "win" ] [ text "" ]
         else
          div [ class "win" ] [ text "You win" ]



buttons : Html Msg
buttons = 
        "abcdefghijklmnopqrstuvwxyz"
        |> String.split ""
        |> List.map (\c -> 
                button [ onClick <| Guess c, class "button"] [ text c ])
        |> div [ class "button-area" ]

welcome : String
welcome = "Welcome to Hangman"

guessFormatter : Model -> String
guessFormatter model =
        model.guessWord
        |> String.split ""
        |> List.map (\c -> if c == " " then
                             " "
                          else
                             letterIfGuessed c model.guessedLetters)
        |> List.foldr (++) ""

letterIfGuessed : String -> Set String -> String
letterIfGuessed letter guessedLetters =
        if Set.member (String.toLower letter) guessedLetters then
           letter
        else
           "_"

-- CSS HELPER

stylesheetLink : String -> Html Msg
stylesheetLink url =
    VirtualDom.node
        "link"
        [ property "rel" (string "stylesheet")
        , property "type" (string "text/css")
        , property "href" (string url)
        ]
        []
