module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Http exposing (Expect, emptyBody, header)
import Json.Decode exposing (Decoder, field, int, list, map3, string)
import Platform.Cmd as Cmd



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Profile =
    { id : Int
    , typ : String
    , fullName : String
    }


type ApiKey
    = Unknown
    | Wise String


type alias Model =
    { apiKey : ApiKey
    , profile : Maybe Profile
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { apiKey = Unknown, profile = Nothing }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeApiKey String
    | GotProfiles (Result Http.Error (List Profile))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeApiKey key ->
            ( { model | apiKey = Wise key }, getPersonalProfile key )

        GotProfiles response ->
            case response of
                Err _ ->
                    ( model, Cmd.none )

                Ok profiles ->
                    ( { model | profile = List.head (List.filter (\p -> p.typ == "PERSONAL") profiles) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Enter your API key", value (myApiKey model.apiKey), onInput ChangeApiKey ] []
        , text
            (case model.profile of
                Nothing ->
                    ""

                Just p ->
                    "Logged in as " ++ p.fullName
            )
        ]


myApiKey : ApiKey -> String
myApiKey apiKey =
    case apiKey of
        Unknown ->
            ""

        Wise key ->
            key



-- HTTP


wiseUrl : String
wiseUrl =
    "https://api.transferwise.com"


profilesApi : String
profilesApi =
    "/v2/profiles"


apiGet :
    { url : String
    , expect : Expect msg
    , token : String
    }
    -> Cmd msg
apiGet req =
    Http.request
        { method = "GET"
        , headers = [ header "Authorization" ("Bearer " ++ req.token) ]
        , url = req.url
        , body = emptyBody
        , expect = req.expect
        , timeout = Nothing
        , tracker = Nothing
        }


getPersonalProfile : String -> Cmd Msg
getPersonalProfile token =
    apiGet { url = wiseUrl ++ profilesApi, expect = Http.expectJson GotProfiles profilesDecoder, token = token }


profilesDecoder : Decoder (List Profile)
profilesDecoder =
    list profileDecoder


profileDecoder : Decoder Profile
profileDecoder =
    map3 Profile
        (field "id" int)
        (field "type" string)
        (field "fullName" string)
