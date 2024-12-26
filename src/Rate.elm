module Rate exposing (Rate, getRate)

-- MODEL

import Api exposing (wiseApiGet)
import Http
import Json.Decode as D
import Url.Builder as B


type alias Rate =
    { source : String
    , target : String
    , rate : Float
    , time : String
    }



-- HTTP


ratesApi : String -> String -> String
ratesApi source target =
    B.absolute [ "v1", "rates" ] [ B.string "source" source, B.string "target" target ]


getRate : String -> String -> String -> (Result Http.Error Rate -> msg) -> Cmd msg
getRate token source target msg =
    wiseApiGet { path = ratesApi source target, expect = Http.expectJson msg (D.index 0 rateDecoder), token = token }


rateDecoder : D.Decoder Rate
rateDecoder =
    D.map4 Rate
        (D.field "source" D.string)
        (D.field "target" D.string)
        (D.field "rate" D.float)
        (D.field "time" D.string)
