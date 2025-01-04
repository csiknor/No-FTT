module Balance exposing (Balance, balancesView, getBalances)

import Api exposing (Status(..), wiseApiGet)
import CSS.Attributes exposing (class, classList)
import CSS.Bootstrap exposing (card, cardBody, cardText, cardTitle, col2, dFlex, flexShrink0, mb3, me3, overflowAuto, spinnerBorder, stretchedLink, textBgPrimary, visuallyHidden)
import Html exposing (Html, a, div, h5, p, span, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder)
import Profile exposing (Profile)
import Url.Builder as B
import Utils exposing (classes)



-- MODEL


type alias Balance =
    { id : Int
    , currency : String
    , name : Maybe String
    , amount : Float
    }



-- VIEW


balancesView : Maybe String -> Status () (List Balance) -> (String -> msg) -> Html msg
balancesView curr status msg =
    case status of
        Loading _ ->
            div [ class spinnerBorder ] [ span [ class visuallyHidden ] [ text "Loading balances..." ] ]

        Loaded balances ->
            div [ classes [ dFlex, overflowAuto, mb3 ] ] <| List.map (balanceView curr msg) balances

        _ ->
            text ""


balanceView : Maybe String -> (String -> msg) -> Balance -> Html msg
balanceView curr msg balance =
    div
        [ classes [ card, flexShrink0, me3, col2 ]
        , classList [ ( textBgPrimary, curr |> Maybe.map ((==) balance.currency) |> Maybe.withDefault False ) ]
        , style "width" "10rem"
        ]
        [ div [ class cardBody ]
            [ h5 [ class cardTitle ] [ text <| Maybe.withDefault balance.currency balance.name ]
            , p [ class cardText ]
                [ text <| String.fromFloat balance.amount
                , a
                    [ href "#"
                    , class stretchedLink
                    , onClick <| msg balance.currency
                    ]
                    []
                ]
            ]
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
