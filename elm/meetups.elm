import Html.App as App exposing (program)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (send,defaultSettings,fromJson, Request, empty)
import Json.Decode exposing (Decoder, float, string, object2, (:=), list)
import Task
import Date exposing (Date, day, fromTime, hour, minute, month, year)
import Ordinal exposing (ordinal, ordinalSuffix)


main = App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

init =
    let
        model = []
    in
        model ! [getMeetups]

-- MODEL

type alias Model = List Meetup
type alias Meetup =
    { name : String
    , time : Float
    }

-- UPDATE


type Msg
    = Fetch
    | ErrorOccurred String
    | DataFetched Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Fetch ->
        model ! [getMeetups]

    ErrorOccurred errorMessage ->
        model ! []

    DataFetched meetups ->
        meetups ! []

getMeetups : Cmd Msg
getMeetups =
    fromJson meetupsDecoder (send defaultSettings getMeetupsRequest)
        |> Task.mapError toString
        |> Task.perform ErrorOccurred DataFetched

getMeetupsRequest : Request
getMeetupsRequest =
    { verb = "GET"
    , headers = [ ("Accept", "application/json")
    ]
    , url = "https://api.meetup.com/PHP-Oxford/events"
    , body = empty
    }

meetupsDecoder : Decoder Model
meetupsDecoder =
    list meetupDecoder

meetupDecoder : Decoder Meetup
meetupDecoder =
    object2 Meetup
        ("name" := string)
        ("time" := float)


-- VIEW

view : Model -> Html Msg
view model =
    div []
        (List.map displayMeetup model)

displayMeetup : Meetup -> Html msg
displayMeetup meetup =
   div [ class "foo" ]
    [ div [] [ text meetup.name ]
    , div [] [ text (displayNiceDate meetup.time) ]
    ]

toDate : Float -> Date
toDate time =
    fromTime time

displayNiceDate : Float -> String
displayNiceDate time =
    let
        date =
            toDate time
    in
        toString (day date)
            ++ ordinalSuffix (day date)
            ++ " "
            ++ toString (month date)
            ++ " "
            ++ toString (year date)
            ++ " at "
            ++ toString (hour date)
            ++ ":"
            ++ toString (minute date)