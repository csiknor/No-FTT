module Main exposing (main)

import Browser
import Html exposing (Html, div, form, input, li, text, ul)
import Html.Attributes as A exposing (name, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..), Expect, emptyBody, header)
import Json.Decode as D exposing (Decoder, at, field, list, map3, map4)
import Json.Encode as E
import Platform.Cmd as Cmd
import Url.Builder as B



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Profile =
    { id : Int
    , typ : String
    , fullName : String
    }


type alias Balance =
    { id : Int
    , currency : String
    , name : Maybe String
    , amount : Float
    }


type alias QuoteForm =
    { currency : Maybe String
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


type ApiState
    = NotConnected
    | Connected String


type Status a
    = NotLoaded
    | Loading
    | Loaded a
    | Failed


type alias Model =
    { error : Maybe String
    , state : ApiState
    , profile : Status Profile
    , balances : Status (List Balance)
    , quoteForm : QuoteForm
    , quote : Status Quote
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing NotConnected NotLoaded NotLoaded (QuoteForm Nothing 100) NotLoaded, Cmd.none )


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



-- UPDATE


type Msg
    = ChangeApiKey String
    | GotProfiles (Result Http.Error (List Profile))
    | GotBalances (Result Http.Error (List Balance))
    | ChangeSourceCurrency String
    | ChangeAmount String
    | SubmitQuote
    | GotQuote (Result Http.Error Quote)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ quoteForm } as model) =
    case ( msg, model.state, model.profile ) of
        ( ChangeApiKey key, _, _ ) ->
            ( { model | state = Connected key, profile = Loading }, getPersonalProfile key )

        ( GotProfiles response, Connected key, _ ) ->
            case response of
                Ok profiles ->
                    profileLoaded model (findPersonalProfile profiles) key

                Err e ->
                    ( err e { model | profile = Failed }, Cmd.none )

        ( GotBalances response, _, _ ) ->
            case response of
                Ok balances ->
                    ( ok { model | balances = Loaded balances }, Cmd.none )

                Err e ->
                    ( err e { model | balances = Failed }, Cmd.none )

        ( ChangeAmount val, _, _ ) ->
            ( { model | quoteForm = { quoteForm | amount = Maybe.withDefault 0 (String.toFloat val) } }, Cmd.none )

        ( ChangeSourceCurrency val, _, _ ) ->
            ( { model | quoteForm = { quoteForm | currency = Just val } }, Cmd.none )

        ( SubmitQuote, Connected key, Loaded profile ) ->
            case model.quoteForm.currency of
                Just curr ->
                    ( { model | quote = Loading }, submitQuote key profile curr model.quoteForm.amount )

                Nothing ->
                    ( { model | error = Just "Invalid quote: missing currency" }, Cmd.none )

        ( GotQuote response, _, _ ) ->
            case response of
                Ok quote ->
                    ( { model | quote = Loaded quote }, Cmd.none )

                Err e ->
                    ( err e { model | quote = Failed }, Cmd.none )

        _ ->
            ( { model | error = Just "Invalid operation" }, Cmd.none )


profileLoaded : Model -> Maybe Profile -> String -> ( Model, Cmd Msg )
profileLoaded model profile key =
    case profile of
        Just p ->
            ( ok { model | profile = Loaded p, balances = Loading }, getBalances key p )

        Nothing ->
            ( { model | error = Just "Personal profile not found" }, Cmd.none )


findPersonalProfile : List Profile -> Maybe Profile
findPersonalProfile profiles =
    List.head (List.filter isPersonalProfile profiles)


isPersonalProfile : Profile -> Bool
isPersonalProfile p =
    p.typ == "PERSONAL"


submitQuote : String -> Profile -> String -> Float -> Cmd Msg
submitQuote key profile currency amount =
    postQuote key
        { profileId = profile.id
        , sourceCurrency = currency
        , targetCurrency = currency
        , sourceAmount = Just amount
        , targetAmount = Nothing
        , preferredPayIn = "BALANCE"
        , targetAccount = Nothing
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
            , [ input [ placeholder "Enter your API key", value (myApiKey model.state), onInput ChangeApiKey ] [] ]
            , loggedInView model
            , balancesView model
            , quoteView model.quote
            ]
        )


errorView : Maybe String -> List (Html msg)
errorView error =
    case error of
        Just msg ->
            textInDiv ("Error occured: " ++ msg)

        Nothing ->
            []


loggedInView : Model -> List (Html msg)
loggedInView model =
    case ( model.state, model.profile ) of
        ( Connected _, Loading ) ->
            textInDiv "Loading profile..."

        ( Connected _, Loaded profile ) ->
            textInDiv ("Logged in as " ++ profile.fullName)

        _ ->
            []


textInDiv : String -> List (Html msg)
textInDiv value =
    [ div [] [ text value ] ]


balancesView : Model -> List (Html Msg)
balancesView model =
    case model.balances of
        Loading ->
            textInDiv "Loading balances..."

        Loaded balances ->
            [ form [ onSubmit SubmitQuote ] (ul [] (List.map balanceView balances) :: quoteFormView model.quoteForm) ]

        _ ->
            []


balanceView : Balance -> Html Msg
balanceView balance =
    li [] [ input [ type_ "radio", name "sourceCurrency", value balance.currency, onInput ChangeSourceCurrency ] [], text (balance.currency ++ " " ++ String.fromFloat balance.amount) ]


quoteFormView : QuoteForm -> List (Html Msg)
quoteFormView quoteForm =
    [ input [ type_ "number", placeholder "Amount", A.min "1", value (String.fromFloat quoteForm.amount), onInput ChangeAmount ] []
    , input [ type_ "submit", value "Submit" ] []
    ]


myApiKey : ApiState -> String
myApiKey model =
    case model of
        NotConnected ->
            ""

        Connected key ->
            key


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


wiseUrl : String
wiseUrl =
    "http://localhost:3000/api"


profilesApi : String
profilesApi =
    "/v2/profiles"


apiGet :
    { url : String
    , expect : Expect msg
    , token : String
    }
    -> Cmd msg
apiGet req =
    Http.request
        { method = "GET"
        , headers = [ header "Authorization" ("Bearer " ++ req.token) ]
        , url = req.url
        , body = emptyBody
        , expect = req.expect
        , timeout = Nothing
        , tracker = Nothing
        }


apiPost :
    { url : String
    , body : Http.Body
    , expect : Expect msg
    , token : String
    }
    -> Cmd msg
apiPost req =
    Http.request
        { method = "POST"
        , headers = [ header "Authorization" ("Bearer " ++ req.token) ]
        , url = req.url
        , body = req.body
        , expect = req.expect
        , timeout = Nothing
        , tracker = Nothing
        }


getPersonalProfile : String -> Cmd Msg
getPersonalProfile token =
    apiGet { url = wiseUrl ++ profilesApi, expect = Http.expectJson GotProfiles (list profileDecoder), token = token }


profileDecoder : Decoder Profile
profileDecoder =
    map3 Profile
        (field "id" D.int)
        (field "type" D.string)
        (field "fullName" D.string)


balancesUrl : Int -> String
balancesUrl id =
    B.crossOrigin wiseUrl [ "v4", "profiles", String.fromInt id, "balances" ] [ B.string "types" "STANDARD" ]


getBalances : String -> Profile -> Cmd Msg
getBalances token profile =
    apiGet { url = balancesUrl profile.id, expect = Http.expectJson GotBalances (list balanceDecoder), token = token }


balanceDecoder : Decoder Balance
balanceDecoder =
    map4 Balance
        (field "id" D.int)
        (field "currency" D.string)
        (field "name" (D.nullable D.string))
        (at [ "amount", "value" ] D.float)


quotesUrl : Int -> String
quotesUrl id =
    B.crossOrigin wiseUrl [ "v3", "profiles", String.fromInt id, "quotes" ] []


postQuote : String -> QuoteReq -> Cmd Msg
postQuote token req =
    apiPost { url = quotesUrl req.profileId, body = Http.jsonBody (quoteReqEncoder req), expect = Http.expectJson GotQuote quoteDecoder, token = token }


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
