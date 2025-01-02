module Profile exposing (Profile, findPersonalProfile, getPersonalProfile, profileView)

import Api exposing (Status(..), wiseApiGet)
import CSS.Attributes exposing (class)
import CSS.Bootstrap exposing (spinnerBorder, spinnerBorderSm, visuallyHidden)
import Html exposing (Html, div, span, text)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, string)
import Utils exposing (classes)



-- MODEL


type alias Profile =
    { id : Int
    , typ : String
    , fullName : String
    }



-- UPDATE


findPersonalProfile : List Profile -> Maybe Profile
findPersonalProfile profiles =
    List.head <| List.filter isPersonalProfile profiles


isPersonalProfile : Profile -> Bool
isPersonalProfile p =
    p.typ == "PERSONAL"



-- VIEW


profileView : Status () Profile -> Html msg
profileView status =
    case status of
        Loading _ ->
            div [ classes [ spinnerBorder, spinnerBorderSm ] ] [ span [ class visuallyHidden ] [ text "Loading profile..." ] ]

        Loaded profile ->
            text profile.fullName

        _ ->
            text ""



-- HTTP


profilesApi : String
profilesApi =
    "/v2/profiles"


getPersonalProfile : String -> (Result Http.Error (List Profile) -> msg) -> Cmd msg
getPersonalProfile token msg =
    wiseApiGet { path = profilesApi, expect = Http.expectJson msg (list profileDecoder), token = token }


profileDecoder : Decoder Profile
profileDecoder =
    map3 Profile
        (field "id" int)
        (field "type" string)
        (field "fullName" string)
