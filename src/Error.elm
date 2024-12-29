module Error exposing (errorsView)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- VIEW


errorsView : List String -> msg -> Html msg
errorsView errors msg =
    case errors of
        [] ->
            text ""

        _ ->
            div [] <|
                List.map errorView errors
                    ++ [ div [] [ button [ onClick msg ] [ text "Clear errors" ] ] ]


errorView : String -> Html msg
errorView error =
    div [] [ text <| "Error: " ++ error ]
