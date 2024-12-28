module Api exposing (ApiState(..), Status(..), allLoaded, apiKeyView, changeFirstLoadingToLoaded, httpErrorToString, loadedValues, wiseApiGet, wiseApiPost, wiseApiPut)

import Html exposing (Html, input)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)
import Http exposing (Error(..), Expect, emptyBody, header)



-- MODEL


type ApiState
    = NotConnected (Maybe String)
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


changeFirstLoadingToLoaded : a -> List (Status a) -> List (Status a)
changeFirstLoadingToLoaded v list =
    case list of
        [] ->
            []

        Loading :: rest ->
            Loaded v :: rest

        x :: rest ->
            x :: changeFirstLoadingToLoaded v rest


allLoaded : List (Status a) -> Bool
allLoaded list =
    case list of
        [] ->
            False

        _ ->
            List.all
                (\s ->
                    case s of
                        Loaded _ ->
                            True

                        _ ->
                            False
                )
                list


loadedValues : List (Status a) -> List a
loadedValues =
    List.filterMap
        (\s ->
            case s of
                Loaded v ->
                    Just v

                _ ->
                    Nothing
        )



-- VIEW


apiKeyView : ApiState -> (String -> msg) -> Html msg
apiKeyView state msg =
    input [ type_ "password", placeholder "Enter your API key", value (myApiKey state), onInput msg ] []


myApiKey : ApiState -> String
myApiKey model =
    case model of
        NotConnected maybeKey ->
            Maybe.withDefault "" maybeKey

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
    wiseApiRequest { method = "GET", path = req.path, body = emptyBody, expect = req.expect, token = req.token }


wiseApiPost :
    { path : String
    , body : Http.Body
    , expect : Expect msg
    , token : String
    }
    -> Cmd msg
wiseApiPost req =
    wiseApiRequest { method = "POST", path = req.path, body = req.body, expect = req.expect, token = req.token }


wiseApiPut :
    { path : String
    , body : Http.Body
    , expect : Expect msg
    , token : String
    }
    -> Cmd msg
wiseApiPut req =
    wiseApiRequest { method = "PUT", path = req.path, body = req.body, expect = req.expect, token = req.token }


wiseApiRequest :
    { method : String
    , path : String
    , body : Http.Body
    , expect : Expect msg
    , token : String
    }
    -> Cmd msg
wiseApiRequest req =
    Http.request
        { method = req.method
        , headers = [ header "Authorization" ("Bearer " ++ req.token) ]
        , url = wiseUrl ++ req.path
        , body = req.body
        , expect = req.expect
        , timeout = Nothing
        , tracker = Nothing
        }
