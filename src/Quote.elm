module Quote exposing (Quote, QuoteReq, PaymentOption, quoteView, postQuote)



-- MODEL


import Api exposing (Status(..), wiseApiPost)
import Html exposing (Html, div, text)
import Http
import Json.Decode as D
import Json.Encode as E
import Url.Builder as B
type alias QuoteReq =
    { profileId : Int
    , sourceCurrency : String
    , targetCurrency : String
    , sourceAmount : Maybe Float
    , targetAmount : Maybe Float
    , preferredPayIn : String
    , targetAccount : Maybe Int
    }


type alias Quote =
    { id : String
    , sourceCurrency : String
    , targetCurrency : String
    , sourceAmount : Maybe Float
    , targetAmount : Maybe Float
    , preferredPayIn : String
    , paymentOptions : List PaymentOption
    }


type alias PaymentOption =
    { payIn : String
    , payOut : String
    , feeAmount : Float
    , priceTotalAmount : Float
    }



-- VIEW


quoteView : Status Quote -> Html msg
quoteView status =
    case status of
        Loading ->
            div [] [ text "Loading quote..." ]

        Loaded quote ->
            div [] <|
                [ div [] [ text ("Quote: " ++ quote.id) ]
                , div [] [ text ("Source: " ++ quote.sourceCurrency ++ " " ++ String.fromFloat (Maybe.withDefault 0 quote.sourceAmount)) ]
                , div [] [ text ("Target: " ++ quote.targetCurrency ++ " " ++ String.fromFloat (Maybe.withDefault 0 quote.targetAmount)) ]
                , div [] [ text ("Pay in: " ++ quote.preferredPayIn) ]
                ]
                    ++ (paymentOptionView <| List.head <| List.filter (\p -> p.payIn == quote.preferredPayIn) <| quote.paymentOptions)

        _ ->
            text ""


paymentOptionView : Maybe PaymentOption -> List (Html msg)
paymentOptionView value =
    case value of
        Just option ->
            [ div [] [ text ("Pay out: " ++ option.payOut) ]
            , div [] [ text ("Fee: " ++ String.fromFloat option.feeAmount) ]
            , div [] [ text ("Price: " ++ String.fromFloat option.priceTotalAmount) ]
            ]

        _ ->
            [ div [] [ text "No payment options" ] ]



-- HTTP


quotesUrl : Int -> String
quotesUrl id =
    B.absolute [ "v3", "profiles", String.fromInt id, "quotes" ] []


postQuote : String -> QuoteReq -> (Result Http.Error Quote -> msg) -> Cmd msg
postQuote token req msg =
    wiseApiPost { path = quotesUrl req.profileId, body = Http.jsonBody (quoteReqEncoder req), expect = Http.expectJson msg quoteDecoder, token = token }


quoteReqEncoder : QuoteReq -> E.Value
quoteReqEncoder req =
    E.object
        [ ( "profileId", E.int req.profileId )
        , ( "sourceCurrency", E.string req.sourceCurrency )
        , ( "targetCurrency", E.string req.targetCurrency )
        , ( "sourceAmount", maybeOrNull E.float req.sourceAmount )
        , ( "targetAmount", maybeOrNull E.float req.targetAmount )
        , ( "preferredPayIn", E.string req.preferredPayIn )
        , ( "targetAccount", maybeOrNull E.int req.targetAccount )
        ]


maybeOrNull : (a -> E.Value) -> Maybe a -> E.Value
maybeOrNull encoder value =
    case value of
        Just v ->
            encoder v

        Nothing ->
            E.null


quoteDecoder : D.Decoder Quote
quoteDecoder =
    D.map7 Quote
        (D.field "id" D.string)
        (D.field "sourceCurrency" D.string)
        (D.field "targetCurrency" D.string)
        (D.maybe (D.field "sourceAmount" D.float))
        (D.maybe (D.field "targetAmount" D.float))
        (D.field "preferredPayIn" D.string)
        (D.field "paymentOptions" (D.list paymentOptionDecoder))


paymentOptionDecoder : D.Decoder PaymentOption
paymentOptionDecoder =
    D.map4 PaymentOption
        (D.field "payIn" D.string)
        (D.field "payOut" D.string)
        (D.at [ "fee", "total" ] D.float)
        (D.at [ "price", "total", "value", "amount" ] D.float)