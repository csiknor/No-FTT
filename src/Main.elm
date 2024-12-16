module Main exposing (..)

import Browser
import Html exposing (Html, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Http exposing (Expect, emptyBody, header)
import Json.Decode exposing (Decoder, at, field, float, int, list, map3, map4, nullable, string)
import Platform.Cmd as Cmd
import Url.Builder as B



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Profile =
    { id : Int
    , typ : String
    , fullName : String
    }


type alias Balance =
    { id : Int
    , currency : String
    , name : Maybe String
    , amount : Float
    }


type Model
    = NotConnected
    | Connected String
    | ProfileLoaded String Profile (List Balance)


init : () -> ( Model, Cmd Msg )
init _ =
    ( NotConnected, Cmd.none )



-- UPDATE


type Msg
    = ChangeApiKey String
    | GotProfiles (Result Http.Error (List Profile))
    | GotBalances (Result Http.Error (List Balance))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeApiKey key ->
            connected key

        GotProfiles response ->
            case response of
                Err _ ->
                    ( model, Cmd.none )

                Ok profiles ->
                    case model of
                        NotConnected ->
                            ( model, Cmd.none )

                        Connected key ->
                            profileLoaded key (findPersonalProfile profiles)

                        ProfileLoaded key _ _ ->
                            profileLoaded key (findPersonalProfile profiles)

        GotBalances response ->
            case response of
                Err _ ->
                    ( model, Cmd.none )

                Ok balances ->
                    case model of
                        NotConnected ->
                            ( model, Cmd.none )

                        Connected key ->
                            connected key

                        ProfileLoaded key profile _ ->
                            ( ProfileLoaded key profile balances, Cmd.none )


connected key =
    ( Connected key, getPersonalProfile key )


profileLoaded key profile =
    case profile of
        Just p ->
            ( ProfileLoaded key p [], getBalances key p )

        Nothing ->
            ( NotConnected, Cmd.none )


findPersonalProfile : List Profile -> Maybe Profile
findPersonalProfile profiles =
    List.head (List.filter isPersonalProfile profiles)


isPersonalProfile : Profile -> Bool
isPersonalProfile p =
    p.typ == "PERSONAL"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (input [ placeholder "Enter your API key", value (myApiKey model), onInput ChangeApiKey ] []
            :: loggedInView model
            ++ balancesView model
        )


loggedInView model =
    case model of
        NotConnected ->
            []

        Connected _ ->
            textInDiv "Loading profile..."

        ProfileLoaded _ profile _ ->
            textInDiv ("Logged in as " ++ profile.fullName)


textInDiv value =
    [ div [] [ text value ] ]


balancesView model =
    case model of
        NotConnected ->
            []

        Connected _ ->
            []

        ProfileLoaded _ _ [] ->
            textInDiv "Loading balances..."

        ProfileLoaded _ _ balances ->
            [ ul [] (List.map balanceView balances) ]


balanceView : Balance -> Html msg
balanceView balance =
    li [] [ text (balance.currency ++ " " ++ String.fromFloat balance.amount) ]


myApiKey model =
    case model of
        NotConnected ->
            ""

        Connected key ->
            key

        ProfileLoaded key _ _ ->
            key



-- HTTP


wiseUrl : String
wiseUrl =
    "http://localhost:3000/api"


profilesApi : String
profilesApi =
    "/v2/profiles"


apiGet :
    { url : String
    , expect : Expect msg
    , token : String
    }
    -> Cmd msg
apiGet req =
    Http.request
        { method = "GET"
        , headers = [ header "Authorization" ("Bearer " ++ req.token) ]
        , url = req.url
        , body = emptyBody
        , expect = req.expect
        , timeout = Nothing
        , tracker = Nothing
        }


getPersonalProfile : String -> Cmd Msg
getPersonalProfile token =
    apiGet { url = wiseUrl ++ profilesApi, expect = Http.expectJson GotProfiles (list profileDecoder), token = token }


profileDecoder : Decoder Profile
profileDecoder =
    map3 Profile
        (field "id" int)
        (field "type" string)
        (field "fullName" string)


balancesUrl : Int -> String
balancesUrl id =
    B.crossOrigin wiseUrl [ "v4", "profiles", String.fromInt id, "balances" ] [ B.string "types" "STANDARD" ]


getBalances : String -> Profile -> Cmd Msg
getBalances token profile =
    apiGet { url = balancesUrl profile.id, expect = Http.expectJson GotBalances (list balanceDecoder), token = token }


balanceDecoder : Decoder Balance
balanceDecoder =
    map4 Balance
        (field "id" int)
        (field "currency" string)
        (field "name" (nullable string))
        (at [ "amount", "value" ] float)
