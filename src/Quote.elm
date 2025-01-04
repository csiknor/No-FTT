module Quote exposing (PaymentOption, Quote, QuoteReq, RelativeAmount(..), TransferMethod(..), postQuote, quotesView)

-- MODEL

import Api exposing (Status(..), loadedValues, wiseApiPost, wrapError)
import CSS.Attributes exposing (class)
import CSS.Bootstrap exposing (alert, alertLight, alertLink, collapse, mb3, mt3, tableBordered, tableHover, tableStriped, textTruncate)
import Html exposing (Html, a, div, li, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, colspan, href, id, title)
import Http
import Json.Decode as D
import Json.Encode as E
import String.Interpolate exposing (interpolate)
import Url.Builder as B
import Utils exposing (classes)


type TransferMethod
    = Balance


type alias QuoteReq =
    { profileId : Int
    , sourceCurrency : String
    , targetCurrency : String
    , amount : RelativeAmount
    , preferredPayIn : TransferMethod
    , targetAccount : Maybe Int
    }


type RelativeAmount
    = SourceAmount Float
    | TargetAmount Float


type alias Quote =
    { id : String
    , sourceAmount : Amount
    , targetAmount : Maybe Amount
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
    , sourceAmount : Amount
    , targetAmount : Amount
    , priceTotalAmount : Amount
    }


type alias Amount =
    { value : Float
    , currency : String
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


quotesView : List (Status QuoteReq Quote) -> Html msg
quotesView list =
    case list of
        [] ->
            text ""

        _ ->
            div [ classes [ alert, alertLight, mb3 ] ]
                [ quoteSummaryView list
                , a [ classes [ alertLink ], href "#quoteList", attribute "data-bs-toggle" "collapse" ] [ text "Details" ]
                , div [ classes [ collapse, mt3 ], id "quoteList" ] [ quoteDetailsView list ]
                ]


quoteSummaryView : List (Status QuoteReq Quote) -> Html msg
quoteSummaryView list =
    let
        loaded =
            loadedValues list
    in
    text <|
        interpolate "Created {0}/{1} quotes of sum amount {2} and total price {3}. "
            [ String.fromInt <| List.length loaded
            , String.fromInt <| List.length list
            , loaded
                |> List.map (.sourceAmount >> .value)
                |> List.sum
                |> String.fromFloat
            , loaded
                |> List.map preferredActivePaymentOption
                |> List.filterMap identity
                |> List.map (.priceTotalAmount >> .value)
                |> List.sum
                |> String.fromFloat
            ]


preferredActivePaymentOption : Quote -> Maybe PaymentOption
preferredActivePaymentOption quote =
    List.head <| List.filter (\p -> p.payIn == quote.preferredPayIn && p.disabled == False) quote.paymentOptions


quoteDetailsView : List (Status QuoteReq Quote) -> Html msg
quoteDetailsView list =
    table [ classes [ CSS.Bootstrap.table, tableStriped, tableBordered, tableHover ] ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ]
                , th [] [ text "Sent" ]
                , th [] [ text "Received" ]
                , th [] [ text "Price" ]
                , th [] [ text "Delivery" ]
                , th [] [ text "Notices" ]
                ]
            ]
        , tbody [] <| List.map quoteView list
        ]


quoteView : Status QuoteReq Quote -> Html msg
quoteView status =
    case status of
        Loading r ->
            tr [] [ td [ colspan 6 ] [ text <| interpolate "Loading quote {0} {1}..." [ stringFromRelativeAmount r.amount, r.sourceCurrency ] ] ]

        Failed r ->
            tr [] [ td [ colspan 6 ] [ text <| interpolate "Failed to load quote {0} {1}" [ stringFromRelativeAmount r.amount, r.sourceCurrency ] ] ]

        Loaded quote ->
            tr [] <|
                td [] [ div [ attribute "style" "width: 5rem;", class textTruncate ] [ text quote.id ] ]
                    :: (paymentOptionView <| preferredActivePaymentOption quote)
                    ++ [ noticesView quote.notices ]

        _ ->
            text ""


stringFromRelativeAmount : RelativeAmount -> String
stringFromRelativeAmount a =
    case a of
        SourceAmount f ->
            "Source " ++ String.fromFloat f

        TargetAmount f ->
            "Target " ++ String.fromFloat f


stringFromAmount : Amount -> String
stringFromAmount a =
    String.fromFloat a.value ++ " " ++ a.currency


paymentOptionView : Maybe PaymentOption -> List (Html msg)
paymentOptionView value =
    case value of
        Just option ->
            [ td [] [ text <| stringFromAmount option.sourceAmount ]
            , td [] [ text <| stringFromAmount option.targetAmount ]
            , td [] [ text <| stringFromAmount option.priceTotalAmount ]
            , estimatedDeliveryView option.estimatedDelivery option.formattedEstimatedDelivery
            ]

        _ ->
            [ td [ colspan 4 ] [ text "No payment options" ] ]


estimatedDeliveryView : Maybe String -> Maybe String -> Html msg
estimatedDeliveryView estimatedDelivery formattedEstimatedDelivery =
    case ( estimatedDelivery, formattedEstimatedDelivery ) of
        ( Just est, Just form ) ->
            td [ title est ] [ text form ]

        _ ->
            td [] [ text "Unknown" ]


noticesView : List Notice -> Html msg
noticesView notices =
    case notices of
        [] ->
            td [] [ text "" ]

        _ ->
            td [] [ ul [] <| List.map noticeView notices ]


noticeView : Notice -> Html msg
noticeView notice =
    li [] [ text <| noticeTypeView notice.type_, text " ", a [ href notice.link ] [ text notice.text ] ]


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


postQuote : String -> QuoteReq -> (Result ( Http.Error, QuoteReq ) Quote -> msg) -> Cmd msg
postQuote token req msg =
    wiseApiPost
        { path = quotesUrl req.profileId
        , body = Http.jsonBody (quoteReqEncoder req)
        , expect = Http.expectJson (wrapError req msg) quoteDecoder
        , token = token
        }


quoteReqEncoder : QuoteReq -> E.Value
quoteReqEncoder req =
    E.object
        [ ( "profileId", E.int req.profileId )
        , ( "sourceCurrency", E.string req.sourceCurrency )
        , ( "targetCurrency", E.string req.targetCurrency )
        , relativeAmountEncoder req.amount
        , ( "preferredPayIn", transferMethodEncoder req.preferredPayIn )
        , ( "targetAccount", maybeOrNull E.int req.targetAccount )
        ]


relativeAmountEncoder : RelativeAmount -> ( String, E.Value )
relativeAmountEncoder amount =
    case amount of
        SourceAmount f ->
            ( "sourceAmount", E.float f )

        TargetAmount f ->
            ( "targetAmount", E.float f )


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
    D.map6 Quote
        (D.field "id" D.string)
        (amountDecoder "sourceAmount" "sourceCurrency")
        (D.maybe (amountDecoder "targetAmount" "targetCurrency"))
        (D.field "preferredPayIn" D.string)
        (D.field "paymentOptions" (D.list paymentOptionDecoder))
        (D.field "notices" (D.list noticeDecoder))


paymentOptionDecoder : D.Decoder PaymentOption
paymentOptionDecoder =
    D.map8 PaymentOption
        (D.field "disabled" D.bool)
        (D.maybe <| D.field "estimatedDelivery" D.string)
        (D.maybe <| D.field "formattedEstimatedDelivery" D.string)
        (D.field "payIn" D.string)
        (D.field "payOut" D.string)
        (amountDecoder "sourceAmount" "sourceCurrency")
        (amountDecoder "targetAmount" "targetCurrency")
        (D.at [ "price", "total", "value" ] <| amountDecoder "amount" "currency")


amountDecoder : String -> String -> D.Decoder Amount
amountDecoder value currency =
    D.map2 Amount
        (D.field value D.float)
        (D.field currency D.string)


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
