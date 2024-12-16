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
    , targetAmount : Float
    , preferredPayIn : String
    , paymentOptions : List PaymentOption
    }


type alias PaymentOption =
    { payIn : String
    , payOut : String
    , feeAmount : Float
    , priceTotalAmount : Float
    }


type ModelState
    = NotConnected
    | Connected String
    | ProfileLoaded String Profile (List Balance) QuoteForm


type alias Model =
    { error : Maybe String
    , state : ModelState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( ok NotConnected, Cmd.none )


ok : ModelState -> Model
ok state =
    { error = Nothing, state = state }


err : Error -> ModelState -> Model
err error state =
    { error = Just (httpErrorToString error), state = state }


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
update msg model =
    case ( msg, model.state ) of
        ( ChangeApiKey key, _ ) ->
            connected key

        ( GotProfiles response, Connected key ) ->
            case response of
                Err e ->
                    ( err e NotConnected, Cmd.none )

                Ok profiles ->
                    profileLoaded key (findPersonalProfile profiles)

        ( GotBalances response, ProfileLoaded key profile _ quoteForm ) ->
            case response of
                Err e ->
                    ( err e model.state, Cmd.none )

                Ok balances ->
                    ( ok (ProfileLoaded key profile balances quoteForm), Cmd.none )

        ( ChangeAmount val, ProfileLoaded key profile balances quoteForm ) ->
            ( ok (ProfileLoaded key profile balances { quoteForm | amount = Maybe.withDefault 0 (String.toFloat val) }), Cmd.none )

        ( ChangeSourceCurrency val, ProfileLoaded key profile balances quoteForm ) ->
            ( ok (ProfileLoaded key profile balances { quoteForm | currency = Just val }), Cmd.none )

        ( SubmitQuote, ProfileLoaded key profile _ {currency, amount} ) ->
            case currency of
                Just curr ->
                    ( ok model.state, submitQuote key profile curr amount )

                Nothing ->
                    ( { error = Just "Invalid quote: missing currency", state = model.state }, Cmd.none )

        ( GotQuote response, _ ) ->
            case response of
                Err e ->
                    ( err e model.state, Cmd.none )

                Ok _ ->
                    ( ok model.state, Cmd.none )

        ( _, _ ) ->
            ( { error = Just "Invalid operation", state = model.state }, Cmd.none )


connected : String -> ( Model, Cmd Msg )
connected key =
    ( ok (Connected key), getPersonalProfile key )


profileLoaded : String -> Maybe Profile -> ( Model, Cmd Msg )
profileLoaded key profile =
    case profile of
        Just p ->
            ( ok (ProfileLoaded key p [] (QuoteForm Nothing 100)), getBalances key p )

        Nothing ->
            ( ok NotConnected, Cmd.none )


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
        , sourceAmount = Nothing
        , targetAmount = Just amount
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
            , loggedInView model.state
            , balancesView model.state
            ]
        )


errorView : Maybe String -> List (Html msg)
errorView error =
    case error of
        Just msg ->
            textInDiv ("Error occured: " ++ msg)

        Nothing ->
            []


loggedInView : ModelState -> List (Html msg)
loggedInView model =
    case model of
        NotConnected ->
            []

        Connected _ ->
            textInDiv "Loading profile..."

        ProfileLoaded _ profile _ _ ->
            textInDiv ("Logged in as " ++ profile.fullName)


textInDiv : String -> List (Html msg)
textInDiv value =
    [ div [] [ text value ] ]


balancesView : ModelState -> List (Html Msg)
balancesView model =
    case model of
        NotConnected ->
            []

        Connected _ ->
            []

        ProfileLoaded _ _ [] _ ->
            textInDiv "Loading balances..."

        ProfileLoaded _ _ balances quoteForm ->
            [ form [ onSubmit SubmitQuote ] (ul [] (List.map balanceView balances) :: quoteView quoteForm) ]


balanceView : Balance -> Html Msg
balanceView balance =
    li [] [ input [ type_ "radio", name "sourceCurrency", value balance.currency, onInput ChangeSourceCurrency ] [], text (balance.currency ++ " " ++ String.fromFloat balance.amount) ]


quoteView : QuoteForm -> List (Html Msg)
quoteView quoteForm =
    [ input [ type_ "number", placeholder "Amount", A.min "1", value (String.fromFloat quoteForm.amount), onInput ChangeAmount ] []
    , input [ type_ "submit", value "Submit" ] []
    ]


myApiKey : ModelState -> String
myApiKey model =
    case model of
        NotConnected ->
            ""

        Connected key ->
            key

        ProfileLoaded key _ _ _ ->
            key



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
        (field "targetAmount" D.float)
        (field "preferredPayIn" D.string)
        (field "paymentOptions" (D.list paymentOptionDecoder))


paymentOptionDecoder : Decoder PaymentOption
paymentOptionDecoder =
    D.map4 PaymentOption
        (field "payIn" D.string)
        (field "payOut" D.string)
        (at [ "fee", "total" ] D.float)
        (at [ "price", "total", "value", "amount" ] D.float)

