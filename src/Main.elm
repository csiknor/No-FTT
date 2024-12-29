module Main exposing (main)

import Api exposing (ApiState(..), Status(..), allLoaded, apiKeyView, changeFirstLoadingToLoaded, changeFirstMatchingLoadingToFailed, changeFirstMatchingLoadingToLoaded, httpErrorToString, loadedValues)
import Balance exposing (Balance, balancesView, getBalances)
import Browser
import Error exposing (errorView)
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes as A exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..), Expect)
import Platform.Cmd as Cmd
import Prng.Uuid as Uuid exposing (Uuid)
import Profile exposing (Profile, findPersonalProfile, getPersonalProfile, profileView)
import Quote exposing (Quote, QuoteReq, postQuote, quotesView)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Rate exposing (Rate, getRate)
import Recipient exposing (Recipient, getRecipients, recipientsView)
import String.Interpolate exposing (interpolate)
import Transfer exposing (Funding, Transfer, fundingsView, postFunding, postTransfer, putTransferCancel, transfersView)



-- MAIN


main : Program ( Int, List Int ) Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias QuoteForm =
    { currency : Maybe String
    , account : Maybe Int
    , amount : Float
    , limit : Float
    }


type alias TransferForm =
    { reference : String
    }


type alias Model =
    { error : Maybe String
    , seed : Seed
    , state : ApiState
    , profile : Status () Profile
    , balances : Status () (List Balance)
    , recipients : Status () (List Recipient)
    , quoteForm : QuoteForm
    , quotes : List (Status QuoteReq Quote)
    , transferForm : TransferForm
    , transfers : List (Status () Transfer)
    , fundings : List (Status () Funding)
    }


init : ( Int, List Int ) -> ( Model, Cmd Msg )
init ( seed, seedExtension ) =
    ( { error = Nothing
      , seed = initialSeed seed seedExtension
      , state = NotConnected Nothing
      , profile = NotLoaded
      , balances = NotLoaded
      , quoteForm = QuoteForm Nothing Nothing 100 100
      , quotes = []
      , recipients = NotLoaded
      , transferForm = TransferForm ""
      , transfers = []
      , fundings = []
      }
    , Cmd.none
    )


ok : Model -> Model
ok model =
    { model | error = Nothing }


err : Error -> Model -> Model
err error model =
    { model | error = Just (httpErrorToString error) }


withProfile : Model -> Status () Profile -> Model
withProfile model profile =
    { model | profile = profile }


withBalances : Model -> Status () (List Balance) -> Model
withBalances model balances =
    { model | balances = balances }


withQuoteForm : Model -> QuoteForm -> Model
withQuoteForm model quoteForm =
    { model | quoteForm = quoteForm }


addQuote : Model -> Status QuoteReq Quote -> Model
addQuote model quote =
    case quote of
        Loaded q ->
            { model | quotes = changeFirstMatchingLoadingToLoaded (\r -> r.sourceAmount == q.sourceAmount) q model.quotes }

        Failed req ->
            { model | quotes = changeFirstMatchingLoadingToFailed (\r -> r.sourceAmount == req.sourceAmount) model.quotes }

        _ ->
            model


withQuotes : Model -> List (Status QuoteReq Quote) -> Model
withQuotes model quotes =
    { model | quotes = quotes }


addTransfer : Model -> Status () Transfer -> Model
addTransfer model transfer =
    case transfer of
        Loaded t ->
            { model | transfers = changeFirstLoadingToLoaded t model.transfers }

        _ ->
            model


withRecipients : Model -> Status () (List Recipient) -> Model
withRecipients model recipients =
    { model | recipients = recipients }


addFunding : Model -> Status () Funding -> Model
addFunding model funding =
    case funding of
        Loaded f ->
            { model | fundings = changeFirstLoadingToLoaded f model.fundings }

        _ ->
            model


withError : Model -> String -> Model
withError model error =
    { model | error = Just error }


resetQuotes : Model -> Model
resetQuotes model =
    { model
        | error = Nothing
        , quoteForm = QuoteForm Nothing Nothing 100 100
        , quotes = []
        , transferForm = TransferForm ""
        , transfers = []
        , fundings = []
    }



-- UPDATE


type Msg
    = ChangeApiKey String
    | GotProfiles (Result Http.Error (List Profile))
    | GotBalances (Result Http.Error (List Balance))
    | GotRate (Result Http.Error Rate)
    | GotRecipients (Result Http.Error (List Recipient))
    | ChangeSourceCurrency String
    | ChangeTargetAccount String
    | ChangeAmount String
    | ChangeLimit String
    | SubmitQuote
    | ResubmitFailedQuote
    | GotQuote (Result ( Http.Error, QuoteReq ) Quote)
    | ChangeReference String
    | SubmitTransfer
    | GotTransfer (Result Http.Error Transfer)
    | CancelTransfer
    | SubmitFunding
    | GotFunding (Result Http.Error Funding)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ quoteForm, transferForm } as model) =
    case ( msg, model.state, model.profile ) of
        ( ChangeApiKey key, _, _ ) ->
            if Uuid.isValidUuid key then
                ( resetQuotes { model | state = Connected key, profile = Loading (), balances = NotLoaded }
                , getPersonalProfile key GotProfiles
                )

            else
                ( resetQuotes
                    { model
                        | state = NotConnected <| Just key
                        , profile = NotLoaded
                        , balances = NotLoaded
                    }
                , Cmd.none
                )

        ( GotProfiles response, Connected key, _ ) ->
            handleResultAndLoad
                response
                (findPersonalProfile >> Result.fromMaybe "Personal profile not found")
                (withProfile model)
                withBalances
                (getBalances key GotBalances)

        ( GotBalances response, _, _ ) ->
            handleResultAndStop response (withBalances model)

        ( GotRate response, _, _ ) ->
            case response of
                Ok rate ->
                    ( { model | quoteForm = { quoteForm | limit = rateAdjustedLimit rate.rate } }, Cmd.none )

                Err e ->
                    ( err e model, Cmd.none )

        ( GotRecipients response, _, _ ) ->
            handleResultAndStop response (withRecipients model)

        ( ChangeAmount val, _, _ ) ->
            ( { model | quoteForm = { quoteForm | amount = Maybe.withDefault 0 (String.toFloat val) } }, Cmd.none )

        ( ChangeLimit val, _, _ ) ->
            ( { model | quoteForm = { quoteForm | limit = Maybe.withDefault 0 (String.toFloat val) } }, Cmd.none )

        ( ChangeSourceCurrency val, Connected key, Loaded profile ) ->
            ( withQuoteForm (resetQuotes model) { quoteForm | currency = Just val, account = Nothing }
            , Cmd.batch
                [ getRate key "HUF" val GotRate
                , getRecipients key profile.id val GotRecipients
                ]
            )

        ( ChangeTargetAccount val, _, _ ) ->
            case String.toInt val of
                Just acc ->
                    ( withQuoteForm (resetQuotes model) { quoteForm | account = Just acc }, Cmd.none )

                Nothing ->
                    ( { model | error = Just "Invalid recipient" }, Cmd.none )

        ( SubmitQuote, Connected key, Loaded profile ) ->
            case ( model.quoteForm.currency, model.quoteForm.account ) of
                ( Just curr, Just acc ) ->
                    let
                        reqs =
                            List.map
                                (\a ->
                                    { profileId = profile.id
                                    , sourceCurrency = curr
                                    , targetCurrency = curr
                                    , sourceAmount = Just a
                                    , targetAmount = Nothing
                                    , preferredPayIn = Quote.Balance
                                    , targetAccount = Just acc
                                    }
                                )
                            <|
                                chunkAmountByLimit model.quoteForm.amount model.quoteForm.limit
                    in
                    ( withQuotes (withQuoteForm (resetQuotes model) quoteForm) <| List.map Loading reqs
                    , Cmd.batch <| List.map (\r -> postQuote key r GotQuote) reqs
                    )

                _ ->
                    ( { model | error = Just "Invalid quotes: missing input" }, Cmd.none )

        ( ResubmitFailedQuote, Connected key, _ ) ->
            Tuple.mapBoth
                (\quotes -> withQuotes model quotes)
                (\cmds -> Cmd.batch cmds)
            <|
                List.unzip <|
                    List.map
                        (\q ->
                            case q of
                                Failed req ->
                                    ( Loading req, postQuote key req GotQuote )

                                _ ->
                                    ( q, Cmd.none )
                        )
                        model.quotes

        ( GotQuote response, _, _ ) ->
            case response of
                Ok quote ->
                    ( ok <| addQuote model <| Loaded quote, Cmd.none )

                Err ( e, req ) ->
                    ( err e <| addQuote model <| Failed req, Cmd.none )

        ( ChangeReference val, _, _ ) ->
            ( { model | transferForm = { transferForm | reference = val } }, Cmd.none )

        ( SubmitTransfer, Connected key, _ ) ->
            case model.quoteForm.account of
                Just acc ->
                    if allLoaded model.quotes then
                        let
                            ( quoteIdsAndUuids, newSeed ) =
                                generateAndPairUuids model.seed <| List.map .id <| loadedValues model.quotes
                        in
                        ( { model | transfers = List.map (\_ -> Loading ()) quoteIdsAndUuids, seed = newSeed }
                        , submitTransfers key acc quoteIdsAndUuids model.transferForm.reference
                        )

                    else
                        ( { model | error = Just "Invalid Quotes" }, Cmd.none )

                _ ->
                    ( { model | error = Just "Invalid Quotes" }, Cmd.none )

        ( GotTransfer response, _, _ ) ->
            handleResultAndStop response <| addTransfer model

        ( CancelTransfer, Connected key, _ ) ->
            if allLoaded model.transfers then
                ( { model | transfers = List.map (\_ -> Loading ()) model.transfers }
                , cancelTransfers key <| loadedValues model.transfers
                )

            else
                ( { model | error = Just "Invalid Transfers" }, Cmd.none )

        ( SubmitFunding, Connected key, Loaded profile ) ->
            if allLoaded model.transfers then
                ( { model | fundings = List.map (\_ -> Loading ()) model.transfers }
                , submitFundings key profile.id <| loadedValues model.transfers
                )

            else
                ( { model | error = Just "Invalid Transfer" }, Cmd.none )

        ( GotFunding response, _, _ ) ->
            handleResultAndStop response <| addFunding model

        _ ->
            ( { model | error = Just "Invalid operation" }, Cmd.none )


handleResultAndExecute : Result Http.Error a -> (a -> Result String b) -> (Status () b -> Model) -> (Result String b -> Cmd Msg) -> ( Model, Cmd Msg )
handleResultAndExecute response mod with cmd =
    case response of
        Ok value ->
            case mod value of
                Ok v ->
                    ( ok <| with <| Loaded v, cmd (Ok v) )

                Err e ->
                    ( withError (with NotLoaded) e, cmd (Err e) )

        Err e ->
            ( err e <| with <| Failed (), Cmd.none )


handleResultAndStop : Result Http.Error a -> (Status () a -> Model) -> ( Model, Cmd Msg )
handleResultAndStop response with =
    handleResultAndExecute response Ok with (\_ -> Cmd.none)


handleResultAndLoad : Result Http.Error a -> (a -> Result String b) -> (Status () b -> Model) -> (Model -> Status () x -> Model) -> (b -> Cmd Msg) -> ( Model, Cmd Msg )
handleResultAndLoad response mod with with2 cmd =
    handleResultAndExecute response mod (with >> (\model -> with2 model <| Loading ())) <|
        Result.map cmd
            >> Result.withDefault Cmd.none


rateAdjustedLimit : Float -> Float
rateAdjustedLimit rate =
    floor >> toFloat >> (*) 10 <| 50000 * rate / 10


chunkAmountByLimit : Float -> Float -> List Float
chunkAmountByLimit amount limit =
    let
        chunks =
            floor (amount / limit)

        remainder =
            amount - (toFloat chunks * limit)
    in
    List.repeat chunks limit
        ++ (if remainder > 0 then
                [ remainder ]

            else
                []
           )


generateAndPairUuids : Seed -> List a -> ( List ( a, Uuid ), Seed )
generateAndPairUuids start list =
    List.foldr
        (\item ( acc, seed ) ->
            step Uuid.generator seed
                |> Tuple.mapFirst (\uuid -> ( item, uuid ) :: acc)
        )
        ( [], start )
        list


submitTransfers : String -> Int -> List ( String, Uuid ) -> String -> Cmd Msg
submitTransfers key targetAccount quoteAndTransactionIds reference =
    Cmd.batch <|
        List.indexedMap
            (\i ( quoteId, transactionId ) ->
                postTransfer key
                    { targetAccount = targetAccount
                    , quoteUuid = quoteId
                    , customerTransactionId = Uuid.toString transactionId
                    , reference =
                        interpolate
                            (reference ++ " {0}/{1}")
                            [ String.fromInt (i + 1), String.fromInt (List.length quoteAndTransactionIds) ]
                    }
                    GotTransfer
            )
            quoteAndTransactionIds


cancelTransfers : String -> List Transfer -> Cmd Msg
cancelTransfers key transfers =
    Cmd.batch <|
        List.map
            (\t -> putTransferCancel key t.id GotTransfer)
            transfers


submitFundings : String -> Int -> List Transfer -> Cmd Msg
submitFundings key profileId transfers =
    Cmd.batch
        (List.map
            (\t -> postFunding key profileId t.id GotFunding)
            transfers
        )



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
        , quotesView model.quotes
        , transferFormView model
        , transfersView model.transfers
        , fundingFormView model
        , fundingsView model.fundings
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
                                [ input [ type_ "number", placeholder "Amount", A.min "1", value (String.fromFloat model.quoteForm.amount), onInput ChangeAmount ] []
                                , input [ type_ "number", placeholder "Limit", A.min "1", value (String.fromFloat model.quoteForm.limit), onInput ChangeLimit ] []
                                , input [ type_ "submit", value "Split & Quote" ] []
                                ]

                            _ ->
                                []
                       )
                    ++ (if
                            List.any
                                (\s ->
                                    case s of
                                        Failed _ ->
                                            True

                                        _ ->
                                            False
                                )
                                model.quotes
                        then
                            [ button [ type_ "button", onClick ResubmitFailedQuote ] [ text "Retry failed" ] ]

                        else
                            []
                       )

        _ ->
            text ""


transferFormView : Model -> Html Msg
transferFormView model =
    case model.transfers of
        [] ->
            if allLoaded model.quotes then
                form [ onSubmit SubmitTransfer ] <|
                    [ input [ type_ "text", placeholder "Reference", value model.transferForm.reference, onInput ChangeReference ] []
                    , input [ type_ "submit", value "Transfer" ] []
                    ]

            else
                text ""

        _ ->
            text ""


fundingFormView : Model -> Html Msg
fundingFormView model =
    case model.fundings of
        [] ->
            if allLoaded model.transfers then
                form [ onSubmit SubmitFunding ] <|
                    [ input [ type_ "submit", value "Fund" ] []
                    , button [ type_ "button", onClick CancelTransfer ] [ text "Cancel" ]
                    ]

            else
                text ""

        _ ->
            text ""
