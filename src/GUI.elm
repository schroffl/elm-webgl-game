module GUI exposing (Model, Msg(Connected), init, view, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Network


type alias Model =
    { username : String
    , url : String
    , connected : Bool
    }


type Msg
    = UsernameInput String
    | UrlInput String
    | Connect
    | Connected


init : Model
init =
    { username = ""
    , url = "ws://"
    , connected = False
    }


view : Model -> Html Msg
view model =
    div [ class "gui" ]
        [ div [ class "menu", hidden model.connected ]
            [ div []
                [ input
                    [ type_ "text"
                    , onInput UrlInput
                    , value model.url
                    , placeholder "Url"
                    ]
                    []
                , input
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

        UrlInput newUrl ->
            ( { model | url = newUrl }, Cmd.none )

        Connect ->
            ( model, Network.connect model.url model.username )

        Connected ->
            ( { model | connected = True }, Cmd.none )
