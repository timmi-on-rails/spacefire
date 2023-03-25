module Mouse exposing (MouseMsg, events, update)

import Html
import Html.Events
import Json.Decode


type MouseMsg a
    = Up a
    | Down a
    | Leave a


update : MouseMsg a -> List a -> List a
update mouseMsg buttons =
    case mouseMsg of
        Up x ->
            List.filter ((/=) x) buttons

        Leave x ->
            List.filter ((/=) x) buttons

        Down x ->
            x :: List.filter ((/=) x) buttons


events : (MouseMsg a -> msg) -> a -> List (Html.Attribute msg)
events f a =
    [ Html.Events.onMouseDown (f (Down a))
    , Html.Events.onMouseUp (f (Up a))
    , Html.Events.onMouseLeave (f (Leave a))
    , Html.Events.on "touchstart" (Json.Decode.succeed (f (Down a)))
    , Html.Events.on "touchend" (Json.Decode.succeed (f (Up a)))
    , Html.Events.on "touchcancel" (Json.Decode.succeed (f (Up a)))
    ]
