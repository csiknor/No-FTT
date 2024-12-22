module Main exposing (main)

import Balance exposing (Balance, balancesView, getBalances)
import Browser
import Html exposing (Html, div, form, input, li, text, ul)
import Html.Attributes as A exposing (checked, name, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..), Expect)
import Json.Decode as D exposing (Decoder, at, field, list, map6)
import Json.Encode as E
import Platform.Cmd as Cmd
import Profile exposing (Profile, findPersonalProfile, getPersonalProfile, profileView)
import Url.Builder as B
import Api exposing (ApiState(..), Status(..), apiKeyView, wiseApiGet, wiseApiPost)


-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias QuoteForm =
    { currency : Maybe String
    , account: Maybe Int
    , amount : Float
    }


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


type alias Recipient =
    { id : Int
    , name : String
    , currency : String
    , accountSummary : String
    , longAccountSummary : String
    , ownedByCustomer : Bool
    }


type alias Model =
    { error : Maybe String
    , state : ApiState
    , profile : Status Profile
    , balances : Status (List Balance)
    , quoteForm : QuoteForm
    , quote : Status Quote
    , recipients : Status (List Recipient)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing NotConnected NotLoaded NotLoaded (QuoteForm Nothing Nothing 100) NotLoaded NotLoaded, Cmd.none )


ok : Model -> Model
ok model =
    { model | error = Nothing }


err : Error -> Model -> Model
err error model =
    { model | error = Just (httpErrorToString error) }


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


withProfile : Model -> Status Profile -> Model
withProfile model profile =
    { model | profile = profile }


withBalances : Model -> Status (List Balance) -> Model
withBalances model balances =
    { model | balances = balances }


withQuote : Model -> Status Quote -> Model
withQuote model quote =
    { model | quote = quote }


withRecipients : Model -> Status (List Recipient) -> Model
withRecipients model recipients =
    { model | recipients = recipients }


withError : Model -> String -> Model
withError model error =
    { model | error = Just error }


handleResultAndExecute : Result Http.Error a -> (a -> Result String b) -> (Status b -> Model) -> (Result String b -> Cmd Msg) -> ( Model, Cmd Msg )
handleResultAndExecute response mod with cmd =
    case response of
        Ok value ->
            case mod value of
                Ok v ->
                    ( ok <| with <| Loaded v, cmd (Ok v) )

                Err e ->
                    ( withError (with NotLoaded) e, cmd (Err e) )

        Err e ->
            ( err e <| with Failed, Cmd.none )


handleResultAndStop : Result Http.Error a -> (Status a -> Model) -> ( Model, Cmd Msg )
handleResultAndStop response with =
    handleResultAndExecute response (\a -> Ok a) with (\_ -> Cmd.none)


handleResultAndLoad : Result Http.Error a -> (a -> Result String b) -> (Status b -> Model) -> (Model -> Status x -> Model) -> (b -> Cmd Msg) -> ( Model, Cmd Msg )
handleResultAndLoad response mod with with2 cmd =
    handleResultAndExecute response mod (with >> (\model -> with2 model Loading))
        <| Result.map (\x -> cmd x) >> Result.withDefault Cmd.none



-- UPDATE


type Msg
    = ChangeApiKey String
    | GotProfiles (Result Http.Error (List Profile))
    | GotBalances (Result Http.Error (List Balance))
    | GotRecipients (Result Http.Error (List Recipient))
    | ChangeSourceCurrency String
    | ChangeTargetAccount String
    | ChangeAmount String
    | SubmitQuote
    | GotQuote (Result Http.Error Quote)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ quoteForm } as model) =
    case ( msg, model.state, model.profile ) of
        ( ChangeApiKey key, _, _ ) ->
            ( { model | state = Connected key, profile = Loading }, getPersonalProfile key GotProfiles)

        ( GotProfiles response, Connected key, _ ) ->
            handleResultAndLoad response (findPersonalProfile >> Result.fromMaybe "Personal profile not found") (withProfile model) withBalances (getBalances key GotBalances)

        ( GotBalances response, _, _ ) ->
            handleResultAndStop response (withBalances model)

        ( GotRecipients response, _, _ ) ->
            handleResultAndStop response (withRecipients model)

        ( ChangeAmount val, _, _ ) ->
            ( { model | quoteForm = { quoteForm | amount = Maybe.withDefault 0 (String.toFloat val) } }, Cmd.none )

        ( ChangeSourceCurrency val, Connected key, Loaded profile ) ->
            ( { model | quoteForm = { quoteForm | currency = Just val, account = Nothing } }, getRecipients key profile.id val )

        ( ChangeTargetAccount val, _, _ ) ->
            case String.toInt val of
                Just acc ->
                    ( { model | quoteForm = { quoteForm | account = Just acc } }, Cmd.none )

                Nothing ->
                    ( { model | error = Just "Invalid recipient" }, Cmd.none )

        ( SubmitQuote, Connected key, Loaded profile ) ->
            case (model.quoteForm.currency, model.quoteForm.account) of
                (Just curr, Just acc) ->
                    ( { model | quote = Loading }, submitQuote key profile curr acc model.quoteForm.amount )

                _ ->
                    ( { model | error = Just "Invalid quote: missing input" }, Cmd.none )

        ( GotQuote response, _, _ ) ->
            handleResultAndStop response (withQuote model)

        _ ->
            ( { model | error = Just "Invalid operation" }, Cmd.none )


submitQuote : String -> Profile -> String -> Int -> Float -> Cmd Msg
submitQuote key profile currency account amount =
    postQuote key
        { profileId = profile.id
        , sourceCurrency = currency
        , targetCurrency = currency
        , sourceAmount = Just amount
        , targetAmount = Nothing
        , preferredPayIn = "BALANCE"
        , targetAccount = Just account
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (List.concat
            [ errorView model.error
            , [ apiKeyView model.state ChangeApiKey ]
            , [ profileView model.profile ]
            , [ quoteFormView model ]
            , quoteView model.quote
            ]
        )


errorView : Maybe String -> List (Html msg)
errorView error =
    case error of
        Just msg ->
            textInDiv ("Error occurred: " ++ msg)

        Nothing ->
            []


textInDiv : String -> List (Html msg)
textInDiv value =
    [ div [] [ text value ] ]


quoteFormView : Model -> Html Msg
quoteFormView model =
    case model.state of
        Connected _ ->
            form [ onSubmit SubmitQuote ]
                <| balancesView model.quoteForm.currency model.balances ChangeSourceCurrency
                    :: recipientsView model.quoteForm.account model.recipients
                    ++ amountView model.quoteForm.amount
                    ++ [input [ type_ "submit", value "Submit" ] []]

        _ -> text ""


amountView : Float -> List (Html Msg)
amountView amount =
    [ input [ type_ "number", placeholder "Amount", A.min "1", value (String.fromFloat amount), onInput ChangeAmount ] []
    ]


recipientsView : Maybe Int -> Status (List Recipient) -> List (Html Msg)
recipientsView acc status =
    case status of
        Loading ->
            textInDiv "Loading recipients..."

        Loaded recipients ->
             [ ul [] <| List.map (recipientView acc) recipients ]

        _ ->
            []


recipientView : Maybe Int -> Recipient -> Html Msg
recipientView acc recipient =
    li []
        [ input
            [ type_ "radio"
            , name "targetAccount"
            , checked <| Maybe.map ((==) recipient.id) >> Maybe.withDefault False <| acc
            , value (String.fromInt recipient.id)
            , onInput ChangeTargetAccount
            ]
            []
        , text (recipient.name ++ " " ++ recipient.accountSummary)
        ]


quoteView : Status Quote -> List (Html Msg)
quoteView status =
    case status of
        Loading ->
            textInDiv "Loading quote..."

        Loaded quote ->
            [ div [] [ text ("Quote: " ++ quote.id) ]
            , div [] [ text ("Source: " ++ quote.sourceCurrency ++ " " ++ String.fromFloat (Maybe.withDefault 0 quote.sourceAmount)) ]
            , div [] [ text ("Target: " ++ quote.targetCurrency ++ " " ++ String.fromFloat (Maybe.withDefault 0 quote.targetAmount)) ]
            , div [] [ text ("Pay in: " ++ quote.preferredPayIn) ]
            ]
                ++ (paymentOptionView <| List.head <| List.filter (\p -> p.payIn == quote.preferredPayIn) <| quote.paymentOptions)

        _ ->
            []


paymentOptionView : Maybe PaymentOption -> List (Html Msg)
paymentOptionView value =
    case value of
        Just option ->
            [ div [] [ text ("Pay out: " ++ option.payOut) ]
            , div [] [ text ("Fee: " ++ String.fromFloat option.feeAmount) ]
            , div [] [ text ("Price: " ++ String.fromFloat option.priceTotalAmount) ]
            ]

        _ ->
            textInDiv "No payment options"



-- HTTP


quotesUrl : Int -> String
quotesUrl id =
    B.absolute [ "v3", "profiles", String.fromInt id, "quotes" ] []


postQuote : String -> QuoteReq -> Cmd Msg
postQuote token req =
    wiseApiPost { path = quotesUrl req.profileId, body = Http.jsonBody (quoteReqEncoder req), expect = Http.expectJson GotQuote quoteDecoder, token = token }


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


quoteDecoder : Decoder Quote
quoteDecoder =
    D.map7 Quote
        (field "id" D.string)
        (field "sourceCurrency" D.string)
        (field "targetCurrency" D.string)
        (D.maybe (field "sourceAmount" D.float))
        (D.maybe (field "targetAmount" D.float))
        (field "preferredPayIn" D.string)
        (field "paymentOptions" (D.list paymentOptionDecoder))


paymentOptionDecoder : Decoder PaymentOption
paymentOptionDecoder =
    D.map4 PaymentOption
        (field "payIn" D.string)
        (field "payOut" D.string)
        (at [ "fee", "total" ] D.float)
        (at [ "price", "total", "value", "amount" ] D.float)


recipientsUrl : Int -> String -> String
recipientsUrl profileId currency =
    B.absolute [ "v2", "accounts" ] [ B.string "profileId" (String.fromInt profileId), B.string "currency" currency ]


getRecipients : String -> Int -> String -> Cmd Msg
getRecipients token profileId currency =
    wiseApiGet { path = recipientsUrl profileId currency, expect = Http.expectJson GotRecipients (field "content" <| list recipientDecoder), token = token }


recipientDecoder : Decoder Recipient
recipientDecoder =
    map6 Recipient
        (field "id" D.int)
        (at [ "name", "fullName" ] D.string)
        (field "currency" D.string)
        (field "accountSummary" D.string)
        (field "longAccountSummary" D.string)
        (field "ownedByCustomer" D.bool)