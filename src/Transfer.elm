module Transfer exposing (Funding, FundingStatus(..), Transfer, TransferReq, fundingsView, postFunding, postTransfer, putTransferCancel, transfersView)

import Api exposing (Status(..), wiseApiPost, wiseApiPut)
import Html exposing (Html, div, text)
import Http
import Json.Decode as D
import Json.Encode as E
import Url.Builder as B



-- MODEL


type alias TransferReq =
    { targetAccount : Int
    , quoteUuid : String
    , customerTransactionId : String
    , reference : String
    }


type alias Transfer =
    { id : Int
    , targetAccount : Int
    , quoteUuid : String
    , customerTransactionId : String
    , reference : String
    , status : String
    , created : String
    , hasActiveIssues : Bool
    }


type alias Funding =
    { type_ : String
    , status : FundingStatus
    , errorCode : Maybe String
    }


type FundingStatus
    = Completed
    | Rejected



-- VIEW


transfersView : List (Status Transfer) -> Html msg
transfersView list =
    case list of
        [] ->
            text ""

        _ ->
            div [] <| List.map transferView list


transferView : Status Transfer -> Html msg
transferView status =
    case status of
        Loading ->
            div [] [ text "Loading transfer..." ]

        Loaded transfer ->
            div []
                [ div [] [ text <| "Transfer: " ++ String.fromInt transfer.id ++ " (" ++ transfer.status ++ ")" ]
                , div []
                    [ case transfer.hasActiveIssues of
                        True ->
                            text "Transfer has active issues"

                        False ->
                            text ""
                    ]
                ]

        _ ->
            text ""


fundingsView : List (Status Funding) -> Html msg
fundingsView list =
    case list of
        [] ->
            text ""

        _ ->
            div [] <| List.map fundingView list


fundingView : Status Funding -> Html msg
fundingView fundingStatus =
    case fundingStatus of
        Loading ->
            div [] [ text "Loading funding..." ]

        Loaded funding ->
            div []
                [ div [] [ text <| "Funding: " ++ funding.type_ ++ " (" ++ fundingStatusView funding.status ++ ")" ]
                , fundingErrorCodeView funding.errorCode
                ]

        _ ->
            text ""


fundingStatusView : FundingStatus -> String
fundingStatusView status =
    case status of
        Completed ->
            "Completed"

        Rejected ->
            "Rejected"


fundingErrorCodeView : Maybe String -> Html msg
fundingErrorCodeView code =
    case code of
        Just val ->
            div [] [ text <| "Error: " ++ val ]

        Nothing ->
            text ""



-- HTTP


transfersUrl : String
transfersUrl =
    "/v1/transfers"


postTransfer : String -> TransferReq -> (Result Http.Error Transfer -> msg) -> Cmd msg
postTransfer token req msg =
    wiseApiPost
        { path = transfersUrl
        , body = Http.jsonBody (transferEncoder req)
        , expect = Http.expectJson msg transferDecoder
        , token = token
        }


cancelTransferApi : Int -> String
cancelTransferApi transferId =
    B.absolute [ "v1", "transfers", String.fromInt transferId, "cancel" ] []


putTransferCancel : String -> Int -> (Result Http.Error Transfer -> msg) -> Cmd msg
putTransferCancel token transferId msg =
    wiseApiPut
        { path = cancelTransferApi transferId
        , body = Http.emptyBody
        , expect = Http.expectJson msg transferDecoder
        , token = token
        }


transferEncoder : TransferReq -> E.Value
transferEncoder req =
    E.object
        [ ( "targetAccount", E.int req.targetAccount )
        , ( "quoteUuid", E.string req.quoteUuid )
        , ( "customerTransactionId", E.string req.customerTransactionId )
        , ( "details", E.object [ ( "reference", E.string req.reference ) ] )
        ]


transferDecoder : D.Decoder Transfer
transferDecoder =
    D.map8 Transfer
        (D.field "id" D.int)
        (D.field "targetAccount" D.int)
        (D.field "quoteUuid" D.string)
        (D.field "customerTransactionId" D.string)
        (D.at [ "details", "reference" ] D.string)
        (D.field "status" D.string)
        (D.field "created" D.string)
        (D.field "hasActiveIssues" D.bool)


fundingUrl : Int -> Int -> String
fundingUrl profileId transferId =
    B.absolute [ "v3", "profiles", String.fromInt profileId, "transfers", String.fromInt transferId, "payments" ] []


postFunding : String -> Int -> Int -> (Result Http.Error Funding -> msg) -> Cmd msg
postFunding token profileId transferId msg =
    wiseApiPost
        { path = fundingUrl profileId transferId
        , body = Http.jsonBody (E.object [ ( "type", E.string "BALANCE" ) ])
        , expect = Http.expectJson msg fundingDecoder
        , token = token
        }


fundingDecoder : D.Decoder Funding
fundingDecoder =
    D.map3 Funding
        (D.field "type" D.string)
        (D.field "status" fundingStatusDecoder)
        (D.maybe (D.field "errorCode" D.string))


fundingStatusDecoder : D.Decoder FundingStatus
fundingStatusDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "COMPLETED" ->
                        D.succeed Completed

                    "REJECTED" ->
                        D.succeed Rejected

                    _ ->
                        D.fail "Unknown funding status"
            )
