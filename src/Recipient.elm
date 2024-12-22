module Recipient exposing (Recipient, getRecipients, recipientsView)

import Api exposing (Status(..), wiseApiGet)
import Html exposing (Html, div, input, li, text, ul)
import Html.Attributes exposing (checked, name, type_, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as D
import Url.Builder as B



-- MODEL


type alias Recipient =
    { id : Int
    , name : String
    , currency : String
    , accountSummary : String
    , longAccountSummary : String
    , ownedByCustomer : Bool
    }



-- VIEW


recipientsView : Maybe Int -> Status (List Recipient) -> (String -> msg) -> Html msg
recipientsView acc status msg =
    case status of
        Loading ->
            div [] [ text "Loading recipients..." ]

        Loaded recipients ->
            ul [] <| List.map (recipientView acc msg) recipients

        _ ->
            text ""


recipientView : Maybe Int -> (String -> msg) -> Recipient -> Html msg
recipientView acc msg recipient =
    li []
        [ input
            [ type_ "radio"
            , name "targetAccount"
            , checked <| Maybe.map ((==) recipient.id) >> Maybe.withDefault False <| acc
            , value (String.fromInt recipient.id)
            , onInput msg
            ]
            []
        , text (recipient.name ++ " " ++ recipient.accountSummary)
        ]



-- HTTP


recipientsUrl : Int -> String -> String
recipientsUrl profileId currency =
    B.absolute [ "v2", "accounts" ] [ B.string "profileId" (String.fromInt profileId), B.string "currency" currency ]


getRecipients : String -> Int -> String -> (Result Http.Error (List Recipient) -> msg) -> Cmd msg
getRecipients token profileId currency msg =
    wiseApiGet { path = recipientsUrl profileId currency, expect = Http.expectJson msg (D.field "content" <| D.list recipientDecoder), token = token }


recipientDecoder : D.Decoder Recipient
recipientDecoder =
    D.map6 Recipient
        (D.field "id" D.int)
        (D.at [ "name", "fullName" ] D.string)
        (D.field "currency" D.string)
        (D.field "accountSummary" D.string)
        (D.field "longAccountSummary" D.string)
        (D.field "ownedByCustomer" D.bool)
