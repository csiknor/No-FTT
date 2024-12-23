module Transfer exposing (Transfer, TransferReq, postTransfer, transferView)

import Api exposing (Status(..), wiseApiPost)
import Html exposing (Html, div, text)
import Http
import Json.Decode as D
import Json.Encode as E



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



-- VIEW


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



-- HTTP


transfersUrl : String
transfersUrl =
    "/v1/transfers"


postTransfer : String -> TransferReq -> (Result Http.Error Transfer -> msg) -> Cmd msg
postTransfer token req msg =
    wiseApiPost { path = transfersUrl, body = Http.jsonBody (transferEncoder req), expect = Http.expectJson msg transferDecoder, token = token }


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
