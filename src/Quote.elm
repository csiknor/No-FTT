module Quote exposing (PaymentOption, Quote, QuoteReq, TransferMethod(..), postQuote, quotesView)

-- MODEL

import Api exposing (Status(..), wiseApiPost)
import Html exposing (Html, div, text)
import Html.Attributes exposing (title)
import Http
import Json.Decode as D
import Json.Encode as E
import Url.Builder as B


type TransferMethod
    = Balance


type alias QuoteReq =
    { profileId : Int
    , sourceCurrency : String
    , targetCurrency : String
    , sourceAmount : Maybe Float
    , targetAmount : Maybe Float
    , preferredPayIn : TransferMethod
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
    , notices : List Notice
    }


type alias PaymentOption =
    { disabled : Bool
    , estimatedDelivery : Maybe String
    , formattedEstimatedDelivery : Maybe String
    , payIn : String
    , payOut : String
    , feeAmount : Float
    , priceTotalAmount : Float
    }


type alias Notice =
    { text : String
    , link : String
    , type_ : NoticeType
    }


type NoticeType
    = Info
    | Warning
    | Blocked



-- VIEW


quotesView : List (Status () Quote) -> Html msg
quotesView list =
    case list of
        [] ->
            text ""

        _ ->
            div [] <| List.map quoteView list


quoteView : Status () Quote -> Html msg
quoteView status =
    case status of
        Loading _ ->
            div [] [ text "Loading quote..." ]

        Loaded quote ->
            div [] <|
                [ div [] [ text ("Quote: " ++ quote.id) ]
                , div [] [ text ("Source: " ++ quote.sourceCurrency ++ " " ++ String.fromFloat (Maybe.withDefault 0 quote.sourceAmount)) ]
                , div [] [ text ("Target: " ++ quote.targetCurrency ++ " " ++ String.fromFloat (Maybe.withDefault 0 quote.targetAmount)) ]
                , div [] [ text ("Pay in: " ++ quote.preferredPayIn) ]
                ]
                    ++ (paymentOptionView <| List.head <| List.filter (\p -> p.payIn == quote.preferredPayIn && p.disabled == False) quote.paymentOptions)
                    ++ noticesView quote.notices

        _ ->
            text ""


paymentOptionView : Maybe PaymentOption -> List (Html msg)
paymentOptionView value =
    case value of
        Just option ->
            [ div [] [ text ("Pay out: " ++ option.payOut) ]
            , div [] [ text ("Fee: " ++ String.fromFloat option.feeAmount) ]
            , div [] [ text ("Price: " ++ String.fromFloat option.priceTotalAmount) ]
            , case ( option.estimatedDelivery, option.formattedEstimatedDelivery ) of
                ( Just est, Just form ) ->
                    div [ title est ] [ text ("Estimated delivery: " ++ form) ]

                _ ->
                    div [] [ text "No estimated delivery" ]
            ]

        _ ->
            [ div [] [ text "No payment options" ] ]


noticesView : List Notice -> List (Html msg)
noticesView notices =
    case notices of
        [] ->
            []

        _ ->
            List.map noticeView notices


noticeView : Notice -> Html msg
noticeView notice =
    div [] [ text ("Notice: " ++ noticeTypeView notice.type_ ++ " " ++ notice.text ++ " " ++ notice.link) ]


noticeTypeView : NoticeType -> String
noticeTypeView noticeType =
    case noticeType of
        Info ->
            "ℹ️"

        Warning ->
            "⚠️"

        Blocked ->
            "🛑"



-- HTTP


quotesUrl : Int -> String
quotesUrl id =
    B.absolute [ "v3", "profiles", String.fromInt id, "quotes" ] []


postQuote : String -> QuoteReq -> (Result Http.Error Quote -> msg) -> Cmd msg
postQuote token req msg =
    wiseApiPost
        { path = quotesUrl req.profileId
        , body = Http.jsonBody (quoteReqEncoder req)
        , expect = Http.expectJson msg quoteDecoder
        , token = token
        }


quoteReqEncoder : QuoteReq -> E.Value
quoteReqEncoder req =
    E.object
        [ ( "profileId", E.int req.profileId )
        , ( "sourceCurrency", E.string req.sourceCurrency )
        , ( "targetCurrency", E.string req.targetCurrency )
        , ( "sourceAmount", maybeOrNull E.float req.sourceAmount )
        , ( "targetAmount", maybeOrNull E.float req.targetAmount )
        , ( "preferredPayIn", transferMethodEncoder req.preferredPayIn )
        , ( "targetAccount", maybeOrNull E.int req.targetAccount )
        ]


transferMethodEncoder : TransferMethod -> E.Value
transferMethodEncoder method =
    case method of
        Balance ->
            E.string "BALANCE"


maybeOrNull : (a -> E.Value) -> Maybe a -> E.Value
maybeOrNull encoder value =
    case value of
        Just v ->
            encoder v

        Nothing ->
            E.null


quoteDecoder : D.Decoder Quote
quoteDecoder =
    D.map8 Quote
        (D.field "id" D.string)
        (D.field "sourceCurrency" D.string)
        (D.field "targetCurrency" D.string)
        (D.maybe (D.field "sourceAmount" D.float))
        (D.maybe (D.field "targetAmount" D.float))
        (D.field "preferredPayIn" D.string)
        (D.field "paymentOptions" (D.list paymentOptionDecoder))
        (D.field "notices" (D.list noticeDecoder))


paymentOptionDecoder : D.Decoder PaymentOption
paymentOptionDecoder =
    D.map7 PaymentOption
        (D.field "disabled" D.bool)
        (D.field "estimatedDelivery" (D.maybe D.string))
        (D.field "formattedEstimatedDelivery" (D.maybe D.string))
        (D.field "payIn" D.string)
        (D.field "payOut" D.string)
        (D.at [ "fee", "total" ] D.float)
        (D.at [ "price", "total", "value", "amount" ] D.float)


noticeDecoder : D.Decoder Notice
noticeDecoder =
    D.map3 Notice
        (D.field "text" D.string)
        (D.field "link" D.string)
        (D.field "type" noticeTypeDecoder)


noticeTypeDecoder : D.Decoder NoticeType
noticeTypeDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "INFO" ->
                        D.succeed Info

                    "WARNING" ->
                        D.succeed Warning

                    "BLOCKED" ->
                        D.succeed Blocked

                    _ ->
                        D.fail <| "Unknown notice type: " ++ str
            )
