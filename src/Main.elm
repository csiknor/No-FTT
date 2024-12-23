module Main exposing (main)

import Api exposing (ApiState(..), Status(..), apiKeyView, httpErrorToString)
import Balance exposing (Balance, balancesView, getBalances)
import Browser
import Error exposing (errorView)
import Html exposing (Html, div, form, input, text)
import Html.Attributes as A exposing (placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..), Expect)
import Platform.Cmd as Cmd
import Prng.Uuid as Uuid
import Profile exposing (Profile, findPersonalProfile, getPersonalProfile, profileView)
import Quote exposing (Quote, QuoteReq, postQuote, quoteView)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Recipient exposing (Recipient, getRecipients, recipientsView)
import Transfer exposing (Funding, Transfer, fundingView, postFunding, postTransfer, transferView)



-- MAIN


main : Program ( Int, List Int ) Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias QuoteForm =
    { currency : Maybe String
    , account : Maybe Int
    , amount : Float
    }


type alias TransferForm =
    { reference : String
    }


type alias Model =
    { error : Maybe String
    , seed : Seed
    , state : ApiState
    , profile : Status Profile
    , balances : Status (List Balance)
    , quoteForm : QuoteForm
    , quote : Status Quote
    , recipients : Status (List Recipient)
    , transferForm : TransferForm
    , transfer : Status Transfer
    , funding : Status Funding
    }


init : ( Int, List Int ) -> ( Model, Cmd Msg )
init ( seed, seedExtension ) =
    ( Model
        Nothing
        (initialSeed seed seedExtension)
        NotConnected
        NotLoaded
        NotLoaded
        (QuoteForm Nothing Nothing 100)
        NotLoaded
        NotLoaded
        (TransferForm "")
        NotLoaded
        NotLoaded
    , Cmd.none
    )


ok : Model -> Model
ok model =
    { model | error = Nothing }


err : Error -> Model -> Model
err error model =
    { model | error = Just (httpErrorToString error) }


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


withTransfer : Model -> Status Transfer -> Model
withTransfer model transfer =
    { model | transfer = transfer }


withFunding : Model -> Status Funding -> Model
withFunding model funding =
    { model | funding = funding }


withError : Model -> String -> Model
withError model error =
    { model | error = Just error }



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
    | ChangeReference String
    | SubmitTransfer
    | GotTransfer (Result Http.Error Transfer)
    | SubmitFunding
    | GotFunding (Result Http.Error Funding)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ quoteForm, transferForm } as model) =
    case ( msg, model.state, model.profile ) of
        ( ChangeApiKey key, _, _ ) ->
            ( { model | state = Connected key, profile = Loading }, getPersonalProfile key GotProfiles )

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
            case ( model.quoteForm.currency, model.quoteForm.account ) of
                ( Just curr, Just acc ) ->
                    ( { model | quote = Loading }, submitQuote key profile curr acc model.quoteForm.amount )

                _ ->
                    ( { model | error = Just "Invalid quote: missing input" }, Cmd.none )

        ( GotQuote response, _, _ ) ->
            handleResultAndStop response (withQuote <| withTransfer (withFunding { model | transferForm = TransferForm "" } NotLoaded) NotLoaded)

        ( ChangeReference val, _, _ ) ->
            ( { model | transferForm = { transferForm | reference = val } }, Cmd.none )

        ( SubmitTransfer, Connected key, _ ) ->
            case ( model.quote, model.quoteForm.account ) of
                ( Loaded quote, Just acc ) ->
                    let
                        ( uuid, newSeed ) =
                            step Uuid.generator model.seed
                    in
                    ( { model | transfer = Loading, seed = newSeed }, submitTransfer key acc quote.id (Uuid.toString uuid) model.transferForm.reference )

                _ ->
                    ( { model | error = Just "Invalid Quote" }, Cmd.none )

        ( GotTransfer response, _, _ ) ->
            handleResultAndStop response (\transfer -> { model | transfer = transfer })

        ( SubmitFunding, Connected key, Loaded profile ) ->
            case model.transfer of
                Loaded transfer ->
                    ( { model | funding = Loading }, postFunding key profile.id transfer.id GotFunding )

                _ ->
                    ( { model | error = Just "Invalid Transfer" }, Cmd.none )

        ( GotFunding response, _, _ ) ->
            handleResultAndStop response (\funding -> { model | funding = funding })

        _ ->
            ( { model | error = Just "Invalid operation" }, Cmd.none )


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
    handleResultAndExecute response mod (with >> (\model -> with2 model Loading)) <|
        Result.map (\x -> cmd x)
            >> Result.withDefault Cmd.none


submitQuote : String -> Profile -> String -> Int -> Float -> Cmd Msg
submitQuote key profile currency account amount =
    postQuote key
        { profileId = profile.id
        , sourceCurrency = currency
        , targetCurrency = currency
        , sourceAmount = Just amount
        , targetAmount = Nothing
        , preferredPayIn = Quote.Balance
        , targetAccount = Just account
        }
        GotQuote


submitTransfer : String -> Int -> String -> String -> String -> Cmd Msg
submitTransfer key targetAccount quoteId transactionId reference =
    postTransfer key
        { targetAccount = targetAccount
        , quoteUuid = quoteId
        , customerTransactionId = transactionId
        , reference = reference
        }
        GotTransfer



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ errorView model.error
        , apiKeyView model.state ChangeApiKey
        , profileView model.profile
        , quoteFormView model
        , quoteView model.quote
        , transferFormView model
        , transferView model.transfer
        , fundingFormView model
        , fundingView model.funding
        ]


quoteFormView : Model -> Html Msg
quoteFormView model =
    case model.state of
        Connected _ ->
            form [ onSubmit SubmitQuote ] <|
                [ balancesView model.quoteForm.currency model.balances ChangeSourceCurrency
                , recipientsView model.quoteForm.account model.recipients ChangeTargetAccount
                ]
                    ++ (case model.quoteForm.account of
                            Just _ ->
                                [ amountView model.quoteForm.amount
                                , input [ type_ "submit", value "Quote" ] []
                                ]

                            _ ->
                                []
                       )

        _ ->
            text ""


amountView : Float -> Html Msg
amountView amount =
    input [ type_ "number", placeholder "Amount", A.min "1", value (String.fromFloat amount), onInput ChangeAmount ] []


transferFormView : Model -> Html Msg
transferFormView model =
    case ( model.quote, model.transfer ) of
        ( Loaded _, NotLoaded ) ->
            form [ onSubmit SubmitTransfer ] <|
                [ input [ type_ "text", placeholder "Reference", value model.transferForm.reference, onInput ChangeReference ] []
                , input [ type_ "submit", value "Transfer" ] []
                ]

        _ ->
            text ""


fundingFormView : Model -> Html Msg
fundingFormView model =
    case ( model.transfer, model.funding ) of
        ( Loaded _, NotLoaded ) ->
            form [ onSubmit SubmitFunding ] <|
                [ input [ type_ "submit", value "Fund" ] []
                ]

        _ ->
            text ""
