module Main exposing (..)

import Browser
import Html exposing (Html, div, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type ApiKey
    = Unknown
    | Wise String


type alias Model =
    { apiKey : ApiKey
    }


init : Model
init =
    { apiKey = Unknown }



-- UPDATE


type Msg
    = ChangeApiKey String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeApiKey key ->
            { model | apiKey = Wise key }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Enter your API key", value (myApiKey model.apiKey), onInput ChangeApiKey ] []
        ]


myApiKey : ApiKey -> String
myApiKey apiKey =
    case apiKey of
        Unknown ->
            ""

        Wise key ->
            key
