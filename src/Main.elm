module Main exposing (main)

import Balance exposing (Balance, balancesView, getBalances)
import Browser
import Html exposing (Html, div, form, input, text)
import Html.Attributes as A exposing (placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..), Expect)
import Platform.Cmd as Cmd
import Profile exposing (Profile, findPersonalProfile, getPersonalProfile, profileView)
import Quote exposing (Quote, QuoteReq, postQuote, quoteView)
import Recipient exposing (Recipient, getRecipients, recipientsView)
import Api exposing (ApiState(..), Status(..), apiKeyView)


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
            ( { model | quoteForm = { quoteForm | currency = Just val, account = Nothing } }, getRecipients key profile.id val GotRecipients )

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
        GotQuote



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
            , [ quoteView model.quote ]
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
                    :: recipientsView model.quoteForm.account model.recipients ChangeTargetAccount
                    :: amountView model.quoteForm.amount
                    ++ [input [ type_ "submit", value "Submit" ] []]

        _ -> text ""


amountView : Float -> List (Html Msg)
amountView amount =
    [ input [ type_ "number", placeholder "Amount", A.min "1", value (String.fromFloat amount), onInput ChangeAmount ] []
    ]