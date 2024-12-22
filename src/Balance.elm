module Balance exposing (Balance, balancesView, getBalances)

import Api exposing (Status(..), wiseApiGet)
import Html exposing (Html, div, input, li, text, ul)
import Html.Attributes exposing (checked, name, type_, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as D exposing (Decoder)
import Profile exposing (Profile)
import Url.Builder as B



-- MODEL


type alias Balance =
    { id : Int
    , currency : String
    , name : Maybe String
    , amount : Float
    }



-- VIEW


balancesView : Maybe String -> Status (List Balance) -> (String -> msg) -> Html msg
balancesView curr status msg =
    case status of
        Loading ->
            div [] [ text "Loading balances..." ]

        Loaded balances ->
            ul [] <| List.map (balanceView curr msg) balances

        _ ->
            text ""


balanceView : Maybe String -> (String -> msg) -> Balance -> Html msg
balanceView curr msg balance =
    li []
        [ input
            [ type_ "radio"
            , name "sourceCurrency"
            , value balance.currency
            , checked <| Maybe.map ((==) balance.currency) >> Maybe.withDefault False <| curr
            , onInput msg
            ]
            []
        , text (balance.currency ++ " " ++ String.fromFloat balance.amount)
        ]



-- HTTP


balancesUrl : Int -> String
balancesUrl id =
    B.absolute [ "v4", "profiles", String.fromInt id, "balances" ] [ B.string "types" "STANDARD" ]


getBalances : String -> (Result Http.Error (List Balance) -> msg) -> Profile -> Cmd msg
getBalances token msg profile =
    wiseApiGet { path = balancesUrl profile.id, expect = Http.expectJson msg (D.list balanceDecoder), token = token }


balanceDecoder : Decoder Balance
balanceDecoder =
    D.map4 Balance
        (D.field "id" D.int)
        (D.field "currency" D.string)
        (D.field "name" (D.nullable D.string))
        (D.at [ "amount", "value" ] D.float)