module Profile exposing (Profile, findPersonalProfile, getPersonalProfile, profileView)

import Api exposing (Status(..), wiseApiGet)
import Html exposing (Html, div, text)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, string)



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
            div [] [ text "Loading profile..." ]

        Loaded profile ->
            div [] [ text <| "Logged in as " ++ profile.fullName ]

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
