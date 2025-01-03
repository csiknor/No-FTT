module Api exposing
    ( ApiState(..)
    , Status(..)
    , allLoaded
    , anyFailed
    , apiKeyView
    , changeFirstMatchingLoadingToFailed
    , changeFirstMatchingLoadingToLoaded
    , httpErrorToString
    , loadedValues
    , wiseApiGet
    , wiseApiPost
    , wiseApiPut
    , wrapError
    )

import CSS.Attributes exposing (class)
import CSS.Bootstrap exposing (alignItemsCenter, btn, btnPrimary, formControl, g3, row, rowColsLgAuto, visuallyHidden)
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (autocomplete, for, id, name, placeholder, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..), Expect, emptyBody, header)
import Utils exposing (classes)



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


anyFailed : List (Status a b) -> Bool
anyFailed =
    List.any
        (\s ->
            case s of
                Failed _ ->
                    True

                _ ->
                    False
        )



-- VIEW


apiKeyView : ApiState -> (String -> msg) -> msg -> Html msg
apiKeyView state change submit =
    case state of
        NotConnected maybeKey ->
            form [ classes [ row, rowColsLgAuto, g3, alignItemsCenter ], onSubmit submit ]
                [ div []
                    [ label [ class visuallyHidden, for "username-input" ] [ text "Username" ]
                    , input [ class formControl, id "username-input", name "username", type_ "text", placeholder "Enter your username", required True, autocomplete True ] []
                    ]
                , div []
                    [ label [ class visuallyHidden, for "api-key-input" ] [ text "API key" ]
                    , input [ class formControl, id "api-key-input", name "api-key", type_ "password", placeholder "Enter your API key", value (Maybe.withDefault "" maybeKey), onInput change, required True, autocomplete True ] []
                    ]
                , div [] [ button [ classes [ btn, btnPrimary ], type_ "submit" ] [ text "Connect" ] ]
                ]

        Connected _ ->
            text ""



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
        , timeout = Just 5000
        , tracker = Nothing
        }


wrapError : a -> (Result ( Http.Error, a ) b -> msg) -> Result Http.Error b -> msg
wrapError a msg result =
    case result of
        Ok b ->
            msg (Ok b)

        Err e ->
            msg (Err ( e, a ))
