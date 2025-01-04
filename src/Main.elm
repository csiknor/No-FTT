module Main exposing (main)

import Api exposing (ApiState(..), Status(..), allLoaded, anyFailed, apiKeyView, changeFirstMatchingLoadingToFailed, changeFirstMatchingLoadingToLoaded, httpErrorToString, loadedValues)
import Balance exposing (Balance, balancesView, getBalances)
import Browser
import CSS exposing (className)
import CSS.Attributes exposing (class)
import CSS.Bootstrap exposing (active, alignItemsCenter, btn, btnPrimary, collapse, container, containerFluid, formControl, g3, h1, mb0, mb2, mb3, mbSm0, meAuto, navItem, navLink, navbar, navbarBrand, navbarCollapse, navbarExpandSm, navbarNav, navbarText, navbarToggler, navbarTogglerIcon, row, rowColsMdAuto, visuallyHidden)
import Error exposing (errorsView)
import Html exposing (Html, a, button, div, form, input, label, li, nav, span, text, ul)
import Html.Attributes as A exposing (attribute, for, href, id, placeholder, type_, value)
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
      , transferForm = TransferForm ""
      , transfers = []
      , fundings = []
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
            { model | quotes = changeFirstMatchingLoadingToLoaded (\r -> r.sourceAmount == q.sourceAmount) q model.quotes }

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
        , transferForm = TransferForm ""
        , transfers = []
        , fundings = []
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
    | GotQuote (Result ( Http.Error, QuoteReq ) Quote)
    | ChangeReference String
    | SubmitTransfer
    | ResubmitFailedTransfer
    | GotTransfer (Result ( Http.Error, AnyTransferReq ) Transfer)
    | CancelTransfer
    | SubmitFunding
    | ResubmitFailedFunding
    | GotFunding (Result ( Http.Error, Int ) Funding)
    | GotPending (Result Http.Error (List Transfer))
    | CancelPending
    | GotPendingCancel (Result ( Http.Error, AnyTransferReq ) Transfer)


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

        ( CancelPending, Connected key, _ ) ->
            case model.pending of
                Loaded transfers ->
                    ( { model | pending = Loaded <| List.map (.id >> Loading) <| loadedValues transfers }
                    , Cmd.batch <| List.map (\t -> putTransferCancel key t.id GotPendingCancel) <| loadedValues transfers
                    )

                _ ->
                    ( withError model "Invalid Pending", Cmd.none )

        ( GotPendingCancel response, _, _ ) ->
            case response of
                Ok transfer ->
                    ( addPending model <| Loaded transfer, Cmd.none )

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
                    ( withError model "Invalid quotes: missing input", Cmd.none )

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
                    ( addQuote model <| Loaded quote, Cmd.none )

                Err ( e, req ) ->
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
                        ( { model | transfers = List.map (\r -> Loading <| CreateTransferReq r) reqs, seed = newSeed }
                        , Cmd.batch <| List.map (\r -> postTransfer key r GotTransfer) reqs
                        )

                    else
                        ( withError model "Invalid Quotes", Cmd.none )

                _ ->
                    ( withError model "Invalid Quotes", Cmd.none )

        ( ResubmitFailedTransfer, Connected key, _ ) ->
            Tuple.mapBoth
                (\transfers -> { model | transfers = transfers })
                (\cmds -> Cmd.batch cmds)
            <|
                List.unzip <|
                    List.map
                        (\t ->
                            case t of
                                Failed (CreateTransferReq transferReq) ->
                                    ( Loading (CreateTransferReq transferReq), postTransfer key transferReq GotTransfer )

                                Failed (CancelTransferReq transferId) ->
                                    ( Loading (CancelTransferReq transferId), putTransferCancel key transferId GotTransfer )

                                _ ->
                                    ( t, Cmd.none )
                        )
                        model.transfers

        ( GotTransfer response, _, _ ) ->
            case response of
                Ok transfer ->
                    ( addTransfer model <| Loaded transfer, Cmd.none )

                Err ( e, req ) ->
                    ( addError "Transfer" e <| addTransfer model <| Failed req, Cmd.none )

        ( CancelTransfer, Connected key, _ ) ->
            if allLoaded model.transfers then
                ( { model | transfers = List.map (\t -> Loading <| CancelTransferReq t.id) <| loadedValues model.transfers }
                , cancelTransfers key <| loadedValues model.transfers
                )

            else
                ( withError model "Invalid Transfers", Cmd.none )

        ( SubmitFunding, Connected key, Loaded profile ) ->
            if allLoaded model.transfers then
                ( { model | fundings = List.map (\t -> Loading t.id) <| loadedValues model.transfers }
                , submitFundings key profile.id <| loadedValues model.transfers
                )

            else
                ( withError model "Invalid Transfer", Cmd.none )

        ( ResubmitFailedFunding, Connected key, Loaded profile ) ->
            Tuple.mapBoth
                (\fundings -> { model | fundings = fundings })
                (\cmds -> Cmd.batch cmds)
            <|
                List.unzip <|
                    List.map
                        (\f ->
                            case f of
                                Failed transferId ->
                                    ( Loading transferId, postFunding key profile.id transferId GotFunding )

                                _ ->
                                    ( f, Cmd.none )
                        )
                        model.fundings

        ( GotFunding response, _, _ ) ->
            case response of
                Ok funding ->
                    ( addFunding model <| Loaded funding, Cmd.none )

                Err ( e, transferId ) ->
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
    div [ classes [ container ] ]
        [ headerView <| profileView model.profile
        , errorsView model.errors ClearErrors
        , apiKeyView model.state ChangeApiKey SubmitApiKey
        , pendingTransfersView model.pending CancelPending
        , quoteFormView model
        , quotesView model.quotes
        , transferFormView model
        , transfersView model.transfers
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
                form [ onSubmit SubmitTransfer ] <|
                    [ input [ type_ "text", placeholder "Reference", value model.transferForm.reference, onInput ChangeReference ] []
                    , input [ type_ "submit", value "Transfer" ] []
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
                form [ onSubmit SubmitFunding ] <|
                    [ input [ type_ "submit", value "Fund" ] []
                    , button [ type_ "button", onClick CancelTransfer ] [ text "Cancel" ]
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
