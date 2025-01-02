module Error exposing (errorsView)

import CSS.Bootstrap exposing (alert, alertDanger, alertDismissible, btnClose, fade, listUnstyled, m0, show)
import Html exposing (Html, button, div, li, strong, text, ul)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import Utils exposing (classes)



-- VIEW


errorsView : List String -> msg -> Html msg
errorsView errors msg =
    case errors of
        [] ->
            text ""

        _ ->
            div [ classes [ alert, alertDanger, alertDismissible, fade, show ] ] <|
                [ strong [] [ text "Problem!" ]
                , ul [ classes [ listUnstyled, m0 ] ] <| List.map errorView errors
                , button [ type_ "button", classes [ btnClose ], onClick msg ] []
                ]


errorView : String -> Html msg
errorView error =
    li [] [ text error ]
