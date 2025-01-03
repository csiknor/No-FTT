module Recipient exposing (Recipient, getRecipients, recipientsView)

import Api exposing (Status(..), wiseApiGet)
import CSS.Attributes exposing (class, classList)
import CSS.Bootstrap exposing (active, col6, fwBold, listGroup, listGroupItem, listGroupItemAction, mb3, spinnerBorder, visuallyHidden)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Url.Builder as B
import Utils exposing (classes)



-- MODEL


type alias Recipient =
    { id : Int
    , name : String
    , nickname : Maybe String
    , currency : String
    , accountSummary : String
    , longAccountSummary : String
    , ownedByCustomer : Bool
    }



-- VIEW


recipientsView : Maybe Int -> Status () (List Recipient) -> (String -> msg) -> Html msg
recipientsView acc status msg =
    case status of
        Loading _ ->
            div [ class spinnerBorder ] [ span [ class visuallyHidden ] [ text "Loading recipients..." ] ]

        Loaded recipients ->
            div [ classes [ listGroup, mb3, col6 ] ] <| List.map (recipientView acc msg) recipients

        _ ->
            text ""


recipientView : Maybe Int -> (String -> msg) -> Recipient -> Html msg
recipientView acc msg recipient =
    a
        [ href "#"
        , classes [ listGroupItem, listGroupItemAction ]
        , classList [ ( active, acc |> Maybe.map ((==) recipient.id) |> Maybe.withDefault False ) ]
        , onClick <| msg <| String.fromInt recipient.id
        ]
        [ div [ class fwBold ] [ text <| recipient.name ++ (recipient.nickname |> Maybe.map (\t -> " (" ++ t ++ ")") |> Maybe.withDefault "") ]
        , text recipient.accountSummary
        ]



-- HTTP


recipientsUrl : Int -> String -> String
recipientsUrl profileId currency =
    B.absolute [ "v2", "accounts" ] [ B.string "profileId" (String.fromInt profileId), B.string "currency" currency ]


getRecipients : String -> Int -> String -> (Result Http.Error (List Recipient) -> msg) -> Cmd msg
getRecipients token profileId currency msg =
    wiseApiGet
        { path = recipientsUrl profileId currency
        , expect = Http.expectJson msg (D.field "content" <| D.list recipientDecoder)
        , token = token
        }


recipientDecoder : D.Decoder Recipient
recipientDecoder =
    D.map7 Recipient
        (D.field "id" D.int)
        (D.at [ "name", "fullName" ] D.string)
        (D.maybe <| D.field "nickname" D.string)
        (D.field "currency" D.string)
        (D.field "accountSummary" D.string)
        (D.field "longAccountSummary" D.string)
        (D.field "ownedByCustomer" D.bool)
