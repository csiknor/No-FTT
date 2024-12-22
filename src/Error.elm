module Error exposing (errorView)


import Html exposing (Html, div, text)



-- VIEW


errorView : Maybe String -> Html msg
errorView error =
    case error of
        Just msg ->
            div [] [ text ("Error occurred: " ++ msg) ]

        Nothing ->
            text ""