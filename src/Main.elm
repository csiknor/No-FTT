module Main exposing (main)

import Api exposing (ApiState(..), Status(..), allLoaded, anyFailed, apiKeyView, changeFirstMatchingLoadingToFailed, changeFirstMatchingLoadingToLoaded, httpErrorToString, loadedValues)
import Balance exposing (Balance, balancesView, getBalances)
import Browser
import CSS exposing (className)
import CSS.Attributes exposing (class)
import CSS.Bootstrap exposing (active, alignItemsCenter, btn, btnDanger, btnPrimary, btnSecondary, collapse, container, containerFluid, formControl, formLabel, g3, h1, mb0, mb2, mb3, mbSm0, meAuto, navItem, navLink, navbar, navbarBrand, navbarCollapse, navbarExpandSm, navbarNav, navbarText, navbarToggler, navbarTogglerIcon, row, rowColsMdAuto, visuallyHidden)
import Error exposing (errorsView)
import Html exposing (Html, a, button, div, form, input, label, li, nav, span, text, ul)
import Html.Attributes as A exposing (attribute, for, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..), Expect)
import Platform.Cmd as Cmd
import Prng.Uuid as Uuid exposing (Uuid)
import Process
import Profile exposing (Profile, findPersonalProfile, getPersonalProfile, profileView)
import Quote exposing (Quote, QuoteReq, RelativeAmount(..), postQuote, quotesView)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Rate exposing (Rate, getRate)
import Recipient exposing (Recipient, getRecipients, recipientsView)
import String.Interpolate exposing (interpolate)
import Task
import Transfer exposing (AnyTransferReq(..), Funding, Transfer, TransferReq, fundingsView, getPendingTransfers, pendingTransfersView, postFunding, postTransfer, putTransferCancel, transfersView)
import Utils exposing (classes)



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
    , action : Maybe String
    }


type alias Model =
    { errors : List String
    , seed : Seed
    , state : ApiState
    , profile : Status () Profile
    , balances : Status () (List Balance)
    , recipients : Status () (List Recipient)
    , quoteForm : QuoteForm
    , quotes : List (Status QuoteReq Quote)
    , transferForm : TransferForm
    , transfers : List (Status AnyTransferReq Transfer)
    , fundings : List (Status Int Funding)
    , confirmFunding : Bool
    , pending : Status () (List (Status Int Transfer))
    }


init : ( Int, List Int ) -> ( Model, Cmd Msg )
init ( seed, seedExtension ) =
    ( { errors = []
      , seed = initialSeed seed seedExtension
      , state = NotConnected Nothing
      , profile = NotLoaded
      , balances = NotLoaded
      , quoteForm = QuoteForm Nothing Nothing 100 100
      , quotes = []
      , recipients = NotLoaded
      , transferForm = TransferForm "" Nothing
      , transfers = []
      , fundings = []
      , confirmFunding = False
      , pending = NotLoaded
      }
    , Cmd.none
    )


addError : String -> Error -> Model -> Model
addError prefix error ({ errors } as model) =
    { model | errors = (prefix ++ ": " ++ httpErrorToString error) :: errors }


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
            { model | quotes = changeFirstMatchingLoadingToLoaded (\r -> r.amount == SourceAmount q.sourceAmount.value) q model.quotes }

        Failed req ->
            { model | quotes = changeFirstMatchingLoadingToFailed ((==) req) model.quotes }

        _ ->
            model


withQuotes : Model -> List (Status QuoteReq Quote) -> Model
withQuotes model quotes =
    { model | quotes = quotes }


addTransfer : Model -> Status AnyTransferReq Transfer -> Model
addTransfer model transfer =
    case transfer of
        Loaded t ->
            { model
                | transfers =
                    changeFirstMatchingLoadingToLoaded
                        (\r ->
                            case r of
                                CreateTransferReq transferReq ->
                                    transferReq.quoteUuid == t.quoteUuid

                                CancelTransferReq transferId ->
                                    transferId == t.id
                        )
                        t
                        model.transfers
            }

        Failed req ->
            { model | transfers = changeFirstMatchingLoadingToFailed ((==) req) model.transfers }

        _ ->
            model


withRecipients : Model -> Status () (List Recipient) -> Model
withRecipients model recipients =
    { model | recipients = recipients }


addFunding : Model -> Status Int Funding -> Model
addFunding model funding =
    case funding of
        Loaded f ->
            { model | fundings = changeFirstMatchingLoadingToLoaded ((==) f.transferId) f model.fundings }

        Failed transferId ->
            { model | fundings = changeFirstMatchingLoadingToFailed ((==) transferId) model.fundings }

        _ ->
            model


withError : Model -> String -> Model
withError ({ errors } as model) error =
    { model | errors = error :: errors }


resetQuotes : Model -> Model
resetQuotes model =
    { model
        | quoteForm = QuoteForm Nothing Nothing 100 100
        , quotes = []
        , transferForm = TransferForm "" Nothing
        , transfers = []
        , fundings = []
        , confirmFunding = False
    }


withPending : Model -> Status () (List Transfer) -> Model
withPending model pending =
    { model
        | pending =
            case pending of
                Loaded transfers ->
                    Loaded <| List.map Loaded transfers

                Failed _ ->
                    Failed ()

                Loading _ ->
                    Loading ()

                NotLoaded ->
                    NotLoaded
    }


addPending : Model -> Status AnyTransferReq Transfer -> Model
addPending model transfer =
    case ( model.pending, transfer ) of
        ( Loaded transfers, Loaded t ) ->
            { model | pending = Loaded <| changeFirstMatchingLoadingToLoaded ((==) t.id) t transfers }

        ( Loaded transfers, Failed (CancelTransferReq transferId) ) ->
            { model | pending = Loaded <| changeFirstMatchingLoadingToFailed ((==) transferId) transfers }

        _ ->
            { model | errors = "Invalid Pending" :: model.errors }



-- UPDATE


type Msg
    = ChangeApiKey String
    | SubmitApiKey
    | ClearErrors
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
    | SendQuote Int QuoteReq
    | GotQuote Int (Result ( Http.Error, QuoteReq ) Quote)
    | ChangeReference String
    | SubmitTransfer
    | ResubmitFailedTransfer
    | SendTransfer Int AnyTransferReq
    | GotTransfer Int (Result ( Http.Error, AnyTransferReq ) Transfer)
    | CancelTransfer
    | SubmitFunding
    | DoSubmitFunding
    | ResubmitFailedFunding
    | SendFunding Int Int
    | GotFunding Int (Result ( Http.Error, Int ) Funding)
    | GotPending (Result Http.Error (List Transfer))
    | CancelPending
    | SendPendingCancel Int Int
    | GotPendingCancel Int (Result ( Http.Error, AnyTransferReq ) Transfer)
    | ClearPending


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ quoteForm, transferForm } as model) =
    case ( msg, model.state, model.profile ) of
        ( ChangeApiKey key, _, _ ) ->
            ( resetQuotes
                { model
                    | state = NotConnected <| Just key
                    , profile = NotLoaded
                    , balances = NotLoaded
                }
            , Cmd.none
            )

        ( SubmitApiKey, NotConnected (Just key), _ ) ->
            if Uuid.isValidUuid key then
                ( resetQuotes { model | state = Connected key, profile = Loading (), balances = NotLoaded }
                , Cmd.batch
                    [ getPersonalProfile key GotProfiles
                    , getPendingTransfers key GotPending
                    ]
                )

            else
                ( withError model "Invalid API key", Cmd.none )

        ( ClearErrors, _, _ ) ->
            ( { model | errors = [] }, Cmd.none )

        ( GotProfiles response, Connected key, _ ) ->
            handleResultAndLoad
                "Profiles"
                response
                (findPersonalProfile >> Result.fromMaybe "Personal profile not found")
                (withProfile model)
                withBalances
                (getBalances key GotBalances)

        ( GotPending response, _, _ ) ->
            handleResultAndStop "Pending" response (withPending model)

        ( ClearPending, _, _ ) ->
            ( { model | pending = NotLoaded }, Cmd.none )

        ( CancelPending, Connected key, _ ) ->
            case model.pending of
                Loaded transfers ->
                    ( { model | pending = Loaded <| List.map (.id >> Loading) <| loadedValues transfers }
                    , pacedMessages (SendPendingCancel 0) <| List.map .id <| loadedValues transfers
                    )

                _ ->
                    ( withError model "Invalid Pending", Cmd.none )

        ( SendPendingCancel attempt transferId, Connected key, _ ) ->
            if pendingIsLoading transferId model.pending then
                ( model, putTransferCancel key transferId (GotPendingCancel attempt) )

            else
                ( model, Cmd.none )

        ( GotPendingCancel attempt response, _, _ ) ->
            case response of
                Ok transfer ->
                    ( addPending model <| Loaded transfer, Cmd.none )

                Err ( e, (CancelTransferReq transferId) as req ) ->
                    retryRateLimit attempt
                        e
                        (SendPendingCancel (attempt + 1) transferId)
                        ( model, Cmd.none )
                        ( addError "Pending" e <| addPending model <| Failed req, Cmd.none )

                Err ( e, req ) ->
                    ( addError "Pending" e <| addPending model <| Failed req, Cmd.none )

        ( GotBalances response, _, _ ) ->
            handleResultAndStop "Balances" response (withBalances model)

        ( GotRate response, _, _ ) ->
            case response of
                Ok rate ->
                    ( { model | quoteForm = { quoteForm | limit = rateAdjustedLimit rate.rate } }, Cmd.none )

                Err e ->
                    ( addError "Rate" e model, Cmd.none )

        ( GotRecipients response, _, _ ) ->
            handleResultAndStop "Recipients" response (withRecipients model)

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
                    ( withError model "Invalid recipient", Cmd.none )

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
                                    , amount = SourceAmount a
                                    , preferredPayIn = Quote.Balance
                                    , targetAccount = Just acc
                                    }
                                )
                            <|
                                chunkAmountByLimit model.quoteForm.amount model.quoteForm.limit
                    in
                    ( withQuotes (withQuoteForm (resetQuotes model) quoteForm) <| List.map Loading reqs
                    , pacedMessages (SendQuote 0) reqs
                    )

                _ ->
                    ( withError model "Invalid quotes: missing input", Cmd.none )

        ( ResubmitFailedQuote, Connected _, _ ) ->
            let
                ( quotes, maybeRequests ) =
                    model.quotes
                        |> List.map
                            (\q ->
                                case q of
                                    Failed req ->
                                        ( Loading req, Just req )

                                    _ ->
                                        ( q, Nothing )
                            )
                        |> List.unzip
            in
            ( withQuotes model quotes
            , pacedMessages (SendQuote 0) <| List.filterMap identity maybeRequests
            )

        ( SendQuote attempt req, Connected key, _ ) ->
            if statusIsLoading req model.quotes then
                ( model, postQuote key req (GotQuote attempt) )

            else
                ( model, Cmd.none )

        ( GotQuote attempt response, _, _ ) ->
            case response of
                Ok quote ->
                    ( addQuote model <| Loaded quote, Cmd.none )

                Err ( e, req ) ->
                    retryRateLimit attempt
                        e
                        (SendQuote (attempt + 1) req)
                        ( model, Cmd.none )
                        ( addError "Quote" e <| addQuote model <| Failed req, Cmd.none )

        ( ChangeReference val, _, _ ) ->
            ( { model | transferForm = { transferForm | reference = val } }, Cmd.none )

        ( SubmitTransfer, Connected key, _ ) ->
            case model.quoteForm.account of
                Just acc ->
                    if allLoaded model.quotes then
                        let
                            ( quoteAndTransactionIds, newSeed ) =
                                generateAndPairUuids model.seed <| List.map .id <| loadedValues model.quotes

                            reqs =
                                List.indexedMap
                                    (\i ( quoteId, transactionId ) ->
                                        { targetAccount = acc
                                        , quoteUuid = quoteId
                                        , customerTransactionId = Uuid.toString transactionId
                                        , reference =
                                            interpolate
                                                (model.transferForm.reference ++ " {0}/{1}")
                                                [ String.fromInt (i + 1), String.fromInt (List.length quoteAndTransactionIds) ]
                                        }
                                    )
                                    quoteAndTransactionIds
                        in
                        ( { model
                            | transferForm = { transferForm | action = Just "Created" }
                            , transfers = List.map (\r -> Loading <| CreateTransferReq r) reqs
                            , seed = newSeed
                          }
                        , pacedMessages (CreateTransferReq >> SendTransfer 0) reqs
                        )

                    else
                        ( withError model "Invalid Quotes", Cmd.none )

                _ ->
                    ( withError model "Invalid Quotes", Cmd.none )

        ( ResubmitFailedTransfer, Connected _, _ ) ->
            let
                ( transfers, maybeRequests ) =
                    model.transfers
                        |> List.map
                            (\t ->
                                case t of
                                    Failed req ->
                                        ( Loading req, Just req )

                                    _ ->
                                        ( t, Nothing )
                            )
                        |> List.unzip
            in
            ( { model | transferForm = { transferForm | action = Just "Resubmitted" }, transfers = transfers }
            , pacedMessages (SendTransfer 0) <| List.filterMap identity maybeRequests
            )

        ( SendTransfer attempt req, Connected key, _ ) ->
            if statusIsLoading req model.transfers then
                ( model, sendTransfer key attempt req )

            else
                ( model, Cmd.none )

        ( GotTransfer attempt response, _, _ ) ->
            case response of
                Ok transfer ->
                    ( addTransfer model <| Loaded transfer, Cmd.none )

                Err ( e, req ) ->
                    retryRateLimit attempt
                        e
                        (SendTransfer (attempt + 1) req)
                        ( model, Cmd.none )
                        ( addError "Transfer" e <| addTransfer model <| Failed req, Cmd.none )

        ( CancelTransfer, Connected key, _ ) ->
            if allLoaded model.transfers then
                ( { model
                    | transferForm = { transferForm | action = Just "Cancelled" }
                    , transfers = List.map (\t -> Loading <| CancelTransferReq t.id) <| loadedValues model.transfers
                  }
                , pacedMessages (\t -> SendTransfer 0 <| CancelTransferReq t.id) <| loadedValues model.transfers
                )

            else
                ( withError model "Invalid Transfers", Cmd.none )

        ( SubmitFunding, _, _ ) ->
            ( { model | confirmFunding = True }, Cmd.none )

        ( DoSubmitFunding, Connected key, Loaded profile ) ->
            if allLoaded model.transfers then
                ( { model | fundings = List.map (\t -> Loading t.id) <| loadedValues model.transfers }
                , pacedMessages (SendFunding 0) <| List.map .id <| loadedValues model.transfers
                )

            else
                ( withError model "Invalid Transfer", Cmd.none )

        ( ResubmitFailedFunding, Connected _, Loaded _ ) ->
            let
                ( fundings, maybeTransferIds ) =
                    model.fundings
                        |> List.map
                            (\f ->
                                case f of
                                    Failed transferId ->
                                        ( Loading transferId, Just transferId )

                                    _ ->
                                        ( f, Nothing )
                            )
                        |> List.unzip
            in
            ( { model | fundings = fundings }
            , pacedMessages (SendFunding 0) <| List.filterMap identity maybeTransferIds
            )

        ( SendFunding attempt transferId, Connected key, Loaded profile ) ->
            if statusIsLoading transferId model.fundings then
                ( model, postFunding key profile.id transferId (GotFunding attempt) )

            else
                ( model, Cmd.none )

        ( GotFunding attempt response, Connected key, Loaded profile ) ->
            case response of
                Ok funding ->
                    let
                        updatedModel =
                            addFunding model <| Loaded funding
                    in
                    ( updatedModel
                    , if allLoaded updatedModel.fundings then
                        getBalances key GotBalances profile

                      else
                        Cmd.none
                    )

                Err ( e, transferId ) ->
                    retryRateLimit attempt
                        e
                        (SendFunding (attempt + 1) transferId)
                        ( model, Cmd.none )
                        ( addError "Funding" e <| addFunding model <| Failed transferId, Cmd.none )

        _ ->
            ( withError model "Invalid operation", Cmd.none )


handleResultAndExecute : String -> Result Http.Error a -> (a -> Result String b) -> (Status () b -> Model) -> (Result String b -> Cmd Msg) -> ( Model, Cmd Msg )
handleResultAndExecute prefix response mod with cmd =
    case response of
        Ok value ->
            case mod value of
                Ok v ->
                    ( with <| Loaded v, cmd (Ok v) )

                Err e ->
                    ( withError (with NotLoaded) e, cmd (Err e) )

        Err e ->
            ( addError prefix e <| with <| Failed (), Cmd.none )


handleResultAndStop : String -> Result Http.Error a -> (Status () a -> Model) -> ( Model, Cmd Msg )
handleResultAndStop prefix response with =
    handleResultAndExecute prefix response Ok with (\_ -> Cmd.none)


handleResultAndLoad : String -> Result Http.Error a -> (a -> Result String b) -> (Status () b -> Model) -> (Model -> Status () x -> Model) -> (b -> Cmd Msg) -> ( Model, Cmd Msg )
handleResultAndLoad prefix response mod with with2 cmd =
    handleResultAndExecute prefix response mod (with >> (\model -> with2 model <| Loading ())) <|
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


batchDelayMs : Float
batchDelayMs =
    500


maxRateLimitRetries : Int
maxRateLimitRetries =
    3


pacedMessages : (a -> Msg) -> List a -> Cmd Msg
pacedMessages toMsg items =
    items
        |> List.indexedMap (\index item -> delayedMessage (toFloat index * batchDelayMs) <| toMsg item)
        |> Cmd.batch


delayedMessage : Float -> Msg -> Cmd Msg
delayedMessage delay message =
    Process.sleep delay
        |> Task.perform (\_ -> message)


retryRateLimit : Int -> Http.Error -> Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
retryRateLimit attempt error retryMsg retryResult failureResult =
    if isRateLimit error && attempt < maxRateLimitRetries then
        Tuple.mapSecond (\_ -> delayedMessage (toFloat <| 1000 * (2 ^ attempt)) retryMsg) retryResult

    else
        failureResult


isRateLimit : Http.Error -> Bool
isRateLimit error =
    case error of
        BadStatus 429 ->
            True

        _ ->
            False


statusIsLoading : a -> List (Status a b) -> Bool
statusIsLoading request =
    List.any
        (\status ->
            case status of
                Loading loadingRequest ->
                    loadingRequest == request

                _ ->
                    False
        )


pendingIsLoading : Int -> Status () (List (Status Int Transfer)) -> Bool
pendingIsLoading transferId pending =
    case pending of
        Loaded transfers ->
            statusIsLoading transferId transfers

        _ ->
            False


sendTransfer : String -> Int -> AnyTransferReq -> Cmd Msg
sendTransfer key attempt req =
    case req of
        CreateTransferReq transferReq ->
            postTransfer key transferReq (GotTransfer attempt)

        CancelTransferReq transferId ->
            putTransferCancel key transferId (GotTransfer attempt)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ classes [ container ] ]
        [ headerView <| profileView model.profile
        , errorsView model.errors ClearErrors
        , apiKeyView model.state ChangeApiKey SubmitApiKey
        , pendingTransfersView model.pending CancelPending ClearPending
        , quoteFormView model
        , quotesView model.quotes
        , transferFormView model
        , transfersView model.transferForm.action model.transfers
        , fundingFormView model
        , fundingsView model.fundings
        ]


headerView : Html a -> Html a
headerView content =
    nav [ classes [ navbar, navbarExpandSm, className "bg-body-tertiary", mb3 ] ]
        [ div [ class containerFluid ]
            [ span [ classes [ navbarBrand, mb0, h1 ] ] [ text "No-FTT" ]
            , button [ class navbarToggler, type_ "button", attribute "data-bs-toggle" "collapse", attribute "data-bs-target" "#navbarSupportedContent" ]
                [ span [ class navbarTogglerIcon ] [] ]
            , div [ classes [ collapse, navbarCollapse ], id "navbarSupportedContent" ]
                [ ul [ classes [ navbarNav, meAuto, mb2, mbSm0 ] ]
                    [ li [ class navItem ] [ a [ classes [ navLink, active ], href "#" ] [ text "Home" ] ]
                    , li [ class navItem ] [ a [ class navLink, href "#" ] [ text "About" ] ]
                    ]
                , span [ class navbarText ] [ content ]
                ]
            ]
        ]


quoteFormView : Model -> Html Msg
quoteFormView model =
    case model.state of
        Connected _ ->
            div [] <|
                [ balancesView model.quoteForm.currency model.balances ChangeSourceCurrency
                , recipientsView model.quoteForm.account model.recipients ChangeTargetAccount
                , splitAndQuoteFormView model.quoteForm
                , retryQuotesView model.quotes
                ]

        _ ->
            text ""


splitAndQuoteFormView : QuoteForm -> Html Msg
splitAndQuoteFormView quoteForm =
    case quoteForm.account of
        Just _ ->
            form [ classes [ row, rowColsMdAuto, g3, alignItemsCenter, mb3 ], onSubmit SubmitQuote ]
                [ div []
                    [ label [ class visuallyHidden, for "amount-input" ] [ text "Amount" ]
                    , input [ class formControl, id "amount-input", type_ "number", placeholder "Amount", A.min "1", value (String.fromFloat quoteForm.amount), onInput ChangeAmount ] []
                    ]
                , div []
                    [ label [ class visuallyHidden, for "limit-input" ] [ text "Limit" ]
                    , input [ class formControl, id "limit-input", type_ "number", placeholder "Limit", A.min "1", value (String.fromFloat quoteForm.limit), onInput ChangeLimit ] []
                    ]
                , div [] [ button [ classes [ btn, btnPrimary ], type_ "submit" ] [ text "Split & Quote" ] ]
                ]

        _ ->
            text ""


retryQuotesView : List (Status QuoteReq Quote) -> Html Msg
retryQuotesView quotes =
    if anyFailed quotes then
        div [] [ button [ type_ "button", onClick ResubmitFailedQuote ] [ text "Retry failed" ] ]

    else
        text ""


transferFormView : Model -> Html Msg
transferFormView model =
    if allLoaded model.quotes then
        case model.transfers of
            [] ->
                form [ classes [ row, rowColsMdAuto, g3, alignItemsCenter, mb3 ], onSubmit SubmitTransfer ] <|
                    [ div []
                        [ label [ class visuallyHidden, for "reference-input" ] [ text "Reference" ]
                        , input [ class formControl, id "reference-input", type_ "text", placeholder "Reference", value model.transferForm.reference, onInput ChangeReference ] []
                        ]
                    , div [] [ button [ classes [ btn, btnPrimary ], type_ "submit" ] [ text "Transfer" ] ]
                    ]

            _ ->
                if
                    List.any
                        (\t ->
                            case t of
                                Failed (CreateTransferReq _) ->
                                    True

                                _ ->
                                    False
                        )
                        model.transfers
                then
                    button [ type_ "button", onClick ResubmitFailedTransfer ] [ text "Retry failed" ]

                else
                    text ""

    else
        text ""


fundingFormView : Model -> Html Msg
fundingFormView model =
    case model.fundings of
        [] ->
            if allLoaded model.transfers && List.all (\t -> t.status == "incoming_payment_waiting") (loadedValues model.transfers) then
                if not model.confirmFunding then
                    form [ classes [ row, rowColsMdAuto, g3, alignItemsCenter, mb3 ], onSubmit SubmitFunding ] <|
                        [ div [] [ button [ classes [ btn, btnPrimary ], type_ "submit" ] [ text "Fund" ] ]
                        , div [] [ button [ classes [ btn, btnSecondary ], type_ "button", onClick CancelTransfer ] [ text "Cancel" ] ]
                        ]

                else
                    form [ classes [ row, rowColsMdAuto, g3, alignItemsCenter, mb3 ], onSubmit DoSubmitFunding ] <|
                        [ div [] [ label [ class formLabel ] [ text "Are you sure?" ] ]
                        , div [] [ button [ classes [ btn, btnDanger ], type_ "submit" ] [ text "Confirm" ] ]
                        , div [] [ button [ classes [ btn, btnSecondary ], type_ "button", onClick CancelTransfer ] [ text "Cancel" ] ]
                        ]

            else if
                List.any
                    (\t ->
                        case t of
                            Failed (CancelTransferReq _) ->
                                True

                            _ ->
                                False
                    )
                    model.transfers
            then
                button [ type_ "button", onClick ResubmitFailedTransfer ] [ text "Retry failed cancel" ]

            else
                text ""

        _ ->
            if anyFailed model.fundings then
                button [ type_ "button", onClick ResubmitFailedFunding ] [ text "Retry failed fund" ]

            else
                text ""
