module Main exposing (..)

import WebGL as GL exposing (..)
import WebGL.Settings as GLSettings
import Html exposing (Html, program, div, Attribute)
import Html.Attributes exposing (width, height)
import Html.Events exposing (onClick, on)
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Json.Decode as Decode
import Window
import Task
import Pointer
import Keyboard
import Time
import Entities.Ground as Ground
import Network
import WebSocket
import GUI
import Camera as Cam


type alias Model =
    { size : Window.Size
    , captureMouse : Bool
    , guiModel : GUI.Model
    , camera : Cam.Camera
    , lastKeyboardEvent : ( Keyboard.KeyCode, Bool )
    }


type Msg
    = Resize Window.Size
    | ClickedCanvas
    | MouseMove Cam.MouseMovement
    | KeyChange Bool Keyboard.KeyCode
    | PointerLockState Bool
    | WebSocketMessage String
    | GUIMessage GUI.Msg


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions =
            always <|
                Sub.batch
                    [ Window.resizes Resize
                    , Keyboard.downs (KeyChange True)
                    , Keyboard.ups (KeyChange False)
                    , Pointer.pointerLockChange PointerLockState
                    , WebSocket.listen "ws://127.0.0.1:9160" WebSocketMessage
                    ]
        }


init : ( Model, Cmd Msg )
init =
    ( { size = Window.Size 0 0
      , captureMouse = False
      , guiModel = GUI.init
      , camera = Cam.initialize
      , lastKeyboardEvent = ( -1, False )
      }
    , getInitialWindowSize
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newSize ->
            ( { model
                | size = newSize
                , camera = Cam.updatePerspective newSize model.camera
              }
            , Cmd.none
            )

        ClickedCanvas ->
            ( model, Pointer.lockPointer )

        MouseMove movement ->
            if model.captureMouse then
                ( { model | camera = Cam.applyMouseMovement movement model.camera }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        KeyChange newState keyCode ->
            if not model.captureMouse || ( keyCode, newState ) == model.lastKeyboardEvent then
                ( model, Cmd.none )
            else
                ( { model | lastKeyboardEvent = ( keyCode, newState ) }
                , Cmd.none
                )

        PointerLockState state ->
            ( { model | captureMouse = state }, Cmd.none )

        WebSocketMessage str ->
            ( model, Cmd.none )

        GUIMessage guiMsg ->
            let
                ( newGuiModel, command ) =
                    GUI.update guiMsg model.guiModel
            in
                ( { model | guiModel = newGuiModel }, command )


view : Model -> Html Msg
view model =
    let
        size =
            model.size

        viewMatrix =
            Cam.makeViewMatrix model.camera
    in
        div []
            [ Html.map GUIMessage (GUI.view model.guiModel)
            ]


getInitialWindowSize : Cmd Msg
getInitialWindowSize =
    Task.perform Resize Window.size


onMouseMove : (Cam.MouseMovement -> Msg) -> Attribute Msg
onMouseMove message =
    let
        decoder =
            Decode.map2 (,)
                (Decode.field "movementX" Decode.float)
                (Decode.field "movementY" Decode.float)
    in
        on "mousemove" <| Decode.map message decoder
