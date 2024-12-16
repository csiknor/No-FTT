module Main exposing (main)

import Browser
import Html exposing (Html, div, input, li, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import Http exposing (Error(..), Expect, emptyBody, header)
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


type ModelState
    = NotConnected
    | Connected String
    | ProfileLoaded String Profile (List Balance)


type alias Model =
    { error : Maybe String
    , state : ModelState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( ok NotConnected, Cmd.none )


ok state =
    { error = Nothing, state = state }


err error state =
    { error = Just (httpErrorToString error), state = state }


httpErrorToString e =
    case e of
        BadUrl msg ->
            "Bad URL: " ++ msg

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network error"

        BadStatus msg ->
            "Bad status: " ++ String.fromInt msg

        BadBody msg ->
            "Bad body: " ++ msg



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
                Err e ->
                    ( err e NotConnected, Cmd.none )

                Ok profiles ->
                    case model.state of
                        NotConnected ->
                            ( ok model.state, Cmd.none )

                        Connected key ->
                            profileLoaded key (findPersonalProfile profiles)

                        ProfileLoaded key _ _ ->
                            profileLoaded key (findPersonalProfile profiles)

        GotBalances response ->
            case response of
                Err e ->
                    ( err e model.state, Cmd.none )

                Ok balances ->
                    case model.state of
                        NotConnected ->
                            ( model, Cmd.none )

                        Connected key ->
                            connected key

                        ProfileLoaded key profile _ ->
                            ( ok (ProfileLoaded key profile balances), Cmd.none )


connected key =
    ( ok (Connected key), getPersonalProfile key )


profileLoaded key profile =
    case profile of
        Just p ->
            ( ok (ProfileLoaded key p []), getBalances key p )

        Nothing ->
            ( ok NotConnected, Cmd.none )


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
        (List.concat
            [ errorView model.error
            , [ input [ placeholder "Enter your API key", value (myApiKey model.state), onInput ChangeApiKey ] [] ]
            , loggedInView model.state
            , balancesView model.state
            ]
        )


errorView error =
    case error of
        Just msg ->
            textInDiv ("Error occured: " ++ msg)

        Nothing ->
            []


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
