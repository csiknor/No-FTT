module Api exposing (ApiState(..), Status(..), apiKeyView, httpErrorToString, wiseApiGet, wiseApiPost)

import Html exposing (Html, input)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)
import Http exposing (Error(..), Expect, emptyBody, header)



-- MODEL


type ApiState
    = NotConnected
    | Connected String


type Status a
    = NotLoaded
    | Loading
    | LoadingItems Int a
    | Loaded a
    | Failed



-- UPDATE


httpErrorToString : Error -> String
httpErrorToString e =
    case e of
        BadUrl msg ->
            "Bad URL: " ++ msg

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network error"

        BadStatus msg ->
            "Bad status: " ++ String.fromInt msg

        BadBody msg ->
            "Bad body: " ++ msg



-- VIEW


apiKeyView : ApiState -> (String -> msg) -> Html msg
apiKeyView state msg =
    input [ type_ "password", placeholder "Enter your API key", value (myApiKey state), onInput msg ] []


myApiKey : ApiState -> String
myApiKey model =
    case model of
        NotConnected ->
            ""

        Connected key ->
            key



-- HTTP


wiseUrl : String
wiseUrl =
    "http://localhost:3000/api"


wiseApiGet :
    { path : String
    , expect : Expect msg
    , token : String
    }
    -> Cmd msg
wiseApiGet req =
    Http.request
        { method = "GET"
        , headers = [ header "Authorization" ("Bearer " ++ req.token) ]
        , url = wiseUrl ++ req.path
        , body = emptyBody
        , expect = req.expect
        , timeout = Nothing
        , tracker = Nothing
        }


wiseApiPost :
    { path : String
    , body : Http.Body
    , expect : Expect msg
    , token : String
    }
    -> Cmd msg
wiseApiPost req =
    Http.request
        { method = "POST"
        , headers = [ header "Authorization" ("Bearer " ++ req.token) ]
        , url = wiseUrl ++ req.path
        , body = req.body
        , expect = req.expect
        , timeout = Nothing
        , tracker = Nothing
        }
