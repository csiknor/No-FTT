module Api exposing
    ( ApiState(..)
    , Status(..)
    , allLoaded
    , apiKeyView
    , changeFirstLoadingToLoaded
    , changeFirstMatchingLoading
    , changeFirstMatchingLoadingToFailed
    , changeFirstMatchingLoadingToLoaded
    , httpErrorToString
    , loadedValues
    , wiseApiGet
    , wiseApiPost
    , wiseApiPut
    , wrapError
    )

import Html exposing (Html, input)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)
import Http exposing (Error(..), Expect, emptyBody, header)



-- MODEL


type ApiState
    = NotConnected (Maybe String)
    | Connected String


type Status req res
    = NotLoaded
    | Loading req
    | Loaded res
    | Failed req



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


changeFirstLoadingToLoaded : b -> List (Status a b) -> List (Status a b)
changeFirstLoadingToLoaded v list =
    changeFirstMatchingLoadingToLoaded (\_ -> True) v list


changeFirstMatchingLoadingToLoaded : (a -> Bool) -> b -> List (Status a b) -> List (Status a b)
changeFirstMatchingLoadingToLoaded matching v list =
    changeFirstMatchingLoading matching (\_ -> Loaded v) list


changeFirstMatchingLoadingToFailed : (a -> Bool) -> List (Status a b) -> List (Status a b)
changeFirstMatchingLoadingToFailed matching list =
    changeFirstMatchingLoading matching Failed list


changeFirstMatchingLoading : (a -> Bool) -> (a -> Status a b) -> List (Status a b) -> List (Status a b)
changeFirstMatchingLoading matching new list =
    case list of
        [] ->
            []

        (Loading a) :: rest ->
            if matching a then
                new a :: rest

            else
                Loading a :: changeFirstMatchingLoading matching new rest

        x :: rest ->
            x :: changeFirstMatchingLoading matching new rest


allLoaded : List (Status a b) -> Bool
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


loadedValues : List (Status a b) -> List b
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


wrapError : a -> (Result ( Http.Error, a ) b -> msg) -> Result Http.Error b -> msg
wrapError a msg result =
    case result of
        Ok b ->
            msg (Ok b)

        Err e ->
            msg (Err ( e, a ))
