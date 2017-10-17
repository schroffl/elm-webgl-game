module GUI exposing (Model, Msg(Connected), init, view, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Network


type alias Model =
    { username : String
    , connected : Bool
    }


type Msg
    = UsernameInput String
    | Connect
    | Connected


init : Model
init =
    { username = ""
    , connected = False
    }


view : Model -> Html Msg
view model =
    div [ class "gui" ]
        [ div [ class "menu", hidden model.connected ]
            [ div []
                [ input
                    [ type_ "text"
                    , onInput UsernameInput
                    , value model.username
                    , placeholder "Username"
                    ]
                    []
                , button [ onClick Connect ] [ text "Connect" ]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UsernameInput newValue ->
            ( { model | username = newValue }, Cmd.none )

        Connect ->
            ( model, Network.connect model.username )

        Connected ->
            ( { model | connected = True }, Cmd.none )
