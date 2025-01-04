module Transfer exposing (AnyTransferReq(..), Funding, FundingStatus(..), Transfer, TransferReq, fundingsView, getPendingTransfers, pendingTransfersView, postFunding, postTransfer, putTransferCancel, transfersView)

import Api exposing (Status(..), loadedValues, wiseApiGet, wiseApiPost, wiseApiPut, wrapError)
import CSS.Attributes exposing (class)
import CSS.Bootstrap exposing (alert, alertDismissible, alertLight, alertLink, alertWarning, btn, btnClose, btnWarning, collapse, fade, mb3, mt3, show, spinnerBorder, tableBordered, tableHover, tableStriped, tableWarning, visuallyHidden)
import Html exposing (Html, a, button, div, h5, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, colspan, href, id, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Encode as E
import String.Interpolate exposing (interpolate)
import Url.Builder as B
import Utils exposing (classes)



-- MODEL


type AnyTransferReq
    = CreateTransferReq TransferReq
    | CancelTransferReq Int


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
    { transferId : Int
    , type_ : String
    , status : FundingStatus
    , errorCode : Maybe String
    }


type FundingStatus
    = Completed
    | Rejected



-- VIEW


transfersView : Maybe String -> List (Status AnyTransferReq Transfer) -> Html msg
transfersView maybeAction list =
    case ( maybeAction, list ) of
        ( _, [] ) ->
            text ""

        ( Just action, _ ) ->
            div [ classes [ alert, alertLight, mb3 ] ]
                [ transferSummaryView action list
                , a [ classes [ alertLink ], href "#transferList", attribute "data-bs-toggle" "collapse" ] [ text "Details" ]
                , div [ classes [ collapse, mt3 ], id "transferList" ] [ transferDetailsView list ]
                ]

        _ ->
            text ""


transferSummaryView : String -> List (Status AnyTransferReq Transfer) -> Html msg
transferSummaryView action list =
    text <|
        interpolate "{0} {1}/{2} transfers. "
            [ action
            , String.fromInt <| List.length <| loadedValues list
            , String.fromInt <| List.length list
            ]


transferDetailsView : List (Status AnyTransferReq Transfer) -> Html msg
transferDetailsView list =
    table [ classes [ CSS.Bootstrap.table, tableStriped, tableBordered, tableHover ] ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ]
                , th [] [ text "Status" ]
                , th [] [ text "Active Issues" ]
                ]
            ]
        , tbody [] <| List.map transferView list
        ]


transferView : Status AnyTransferReq Transfer -> Html msg
transferView status =
    case status of
        Loading _ ->
            tr [] [ td [ colspan 3 ] [ text "Loading transfer..." ] ]

        Loaded transfer ->
            loadedTransferView transfer

        Failed (CreateTransferReq req) ->
            tr [] [ td [ colspan 3 ] [ text <| "Failed to create transfer for quote " ++ req.quoteUuid ] ]

        Failed (CancelTransferReq transferId) ->
            tr [] [ td [ colspan 3 ] [ text <| "Failed to cancel transfer " ++ String.fromInt transferId ] ]

        _ ->
            tr [] [ td [ colspan 3 ] [ text "" ] ]


loadedTransferView : Transfer -> Html msg
loadedTransferView transfer =
    tr []
        [ td [] [ text <| String.fromInt transfer.id ]
        , td [] [ text <| transfer.status ]
        , td []
            [ case transfer.hasActiveIssues of
                True ->
                    text "Transfer has active issues"

                False ->
                    text ""
            ]
        ]


pendingTransfersView : Status () (List (Status Int Transfer)) -> msg -> msg -> Html msg
pendingTransfersView pending cancel clear =
    case pending of
        Loading _ ->
            div [ class spinnerBorder ] [ span [ class visuallyHidden ] [ text "Loading pending transfers..." ] ]

        Loaded [] ->
            text ""

        Loaded transfers ->
            div [ classes [ alert, alertWarning, alertDismissible, fade, show ] ] <|
                [ h5 [] [ text "You have pending transfers!" ]
                , pendingTransferDetailsView transfers
                , button [ classes [ btn, btnWarning ], onClick cancel ] [ text "Cancel Pending" ]
                , button [ type_ "button", classes [ btnClose ], attribute "data-bs-dismiss" "alert", onClick clear ] []
                ]

        _ ->
            text ""


pendingTransferDetailsView : List (Status Int Transfer) -> Html msg
pendingTransferDetailsView list =
    table [ classes [ CSS.Bootstrap.table, tableStriped, tableBordered, tableHover, tableWarning ] ]
        [ thead []
            [ tr []
                [ th [] [ text "Id" ]
                , th [] [ text "Status" ]
                , th [] [ text "Active Issues" ]
                ]
            ]
        , tbody [] <| List.map pendingTransferView list
        ]


pendingTransferView : Status Int Transfer -> Html msg
pendingTransferView status =
    case status of
        Loading _ ->
            tr [] [ td [ colspan 3 ] [ text "Cancelling pending transfer..." ] ]

        Loaded transfer ->
            loadedTransferView transfer

        Failed _ ->
            tr [] [ td [ colspan 3 ] [ text "Failed to cancel pending transfer" ] ]

        _ ->
            text ""


fundingsView : List (Status Int Funding) -> Html msg
fundingsView list =
    case list of
        [] ->
            text ""

        _ ->
            div [ classes [ alert, alertLight, mb3 ] ]
                [ fundingSummaryView list
                , a [ classes [ alertLink ], href "#fundingList", attribute "data-bs-toggle" "collapse" ] [ text "Details" ]
                , div [ classes [ collapse, mt3 ], id "fundingList" ] [ fundingDetailsView list ]
                ]


fundingSummaryView : List (Status Int Funding) -> Html msg
fundingSummaryView list =
    let
        loaded =
            loadedValues list
    in
    text <|
        interpolate "Funded {0}/{1} transfers. "
            [ String.fromInt <| List.length loaded
            , String.fromInt <| List.length list
            ]


fundingDetailsView : List (Status Int Funding) -> Html msg
fundingDetailsView list =
    table [ classes [ CSS.Bootstrap.table, tableStriped, tableBordered, tableHover ] ]
        [ thead []
            [ tr []
                [ th [] [ text "Transfer Id" ]
                , th [] [ text "Type" ]
                , th [] [ text "Status" ]
                , th [] [ text "Error Code" ]
                ]
            ]
        , tbody [] <| List.map fundingView list
        ]


fundingView : Status Int Funding -> Html msg
fundingView fundingStatus =
    case fundingStatus of
        Loading _ ->
            tr [] [ td [ colspan 4 ] [ text "Loading funding..." ] ]

        Loaded funding ->
            tr []
                [ td [] [ text <| String.fromInt funding.transferId ]
                , td [] [ text <| funding.type_ ]
                , td [] [ text <| fundingStatusView funding.status ]
                , td [] [ text <| Maybe.withDefault "" funding.errorCode ]
                ]

        Failed transferId ->
            tr [] [ td [ colspan 4 ] [ text <| "Failed to fund transfer " ++ String.fromInt transferId ] ]

        _ ->
            text ""


fundingStatusView : FundingStatus -> String
fundingStatusView status =
    case status of
        Completed ->
            "Completed"

        Rejected ->
            "Rejected"



-- HTTP


transfersUrl : String
transfersUrl =
    "/v1/transfers"


pendingTransfersApi : String
pendingTransfersApi =
    B.absolute [ "v1", "transfers" ] [ B.string "status" "incoming_payment_waiting", B.int "limit" 100 ]


getPendingTransfers : String -> (Result Http.Error (List Transfer) -> msg) -> Cmd msg
getPendingTransfers token msg =
    wiseApiGet
        { path = pendingTransfersApi
        , expect = Http.expectJson msg (D.list transferDecoder)
        , token = token
        }


postTransfer : String -> TransferReq -> (Result ( Http.Error, AnyTransferReq ) Transfer -> msg) -> Cmd msg
postTransfer token req msg =
    wiseApiPost
        { path = transfersUrl
        , body = Http.jsonBody (transferEncoder req)
        , expect = Http.expectJson (wrapError (CreateTransferReq req) msg) transferDecoder
        , token = token
        }


cancelTransferApi : Int -> String
cancelTransferApi transferId =
    B.absolute [ "v1", "transfers", String.fromInt transferId, "cancel" ] []


putTransferCancel : String -> Int -> (Result ( Http.Error, AnyTransferReq ) Transfer -> msg) -> Cmd msg
putTransferCancel token transferId msg =
    wiseApiPut
        { path = cancelTransferApi transferId
        , body = Http.emptyBody
        , expect = Http.expectJson (wrapError (CancelTransferReq transferId) msg) transferDecoder
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


postFunding : String -> Int -> Int -> (Result ( Http.Error, Int ) Funding -> msg) -> Cmd msg
postFunding token profileId transferId msg =
    wiseApiPost
        { path = fundingUrl profileId transferId
        , body = Http.jsonBody (E.object [ ( "type", E.string "BALANCE" ) ])
        , expect = Http.expectJson (wrapError transferId msg) (fundingDecoder transferId)
        , token = token
        }


fundingDecoder : Int -> D.Decoder Funding
fundingDecoder transferId =
    D.map4 Funding
        (D.succeed transferId)
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
