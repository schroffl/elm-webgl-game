module Main exposing (..)

import WebGL as GL exposing (..)
import WebGL.Settings as GLSettings
import Html exposing (Html, program, text, Attribute)
import Html.Attributes exposing (width, height)
import Html.Events exposing (onClick, on)
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Json.Decode as Decode
import Window
import Task
import Pointer
import Keyboard
import AnimationFrame
import Time
import Entities.Ground as Ground
import Entities.Obstacle as Obstacle


type alias Model =
    { size : Window.Size
    , perspectiveMatrix : Mat4
    , position : Vec3
    , velocity : Vec3
    , rotation : Float
    , tilt : Float
    , keys : Keys
    }


type Msg
    = Animate Time.Time
    | Resize Window.Size
    | ClickedCanvas
    | MouseMove MouseMovement
    | KeyChange Bool Keyboard.KeyCode


eyeLevel : Float
eyeLevel =
    1.0


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
                    , AnimationFrame.diffs Animate
                    ]
        }


init : ( Model, Cmd Msg )
init =
    ( { size = Window.Size 0 0
      , perspectiveMatrix = Matrix4.identity
      , position = (vec3 0 eyeLevel 0)
      , velocity = (vec3 0 0 0)
      , rotation = 0
      , tilt = 0
      , keys = Keys False False False False False False
      }
    , getInitialWindowSize
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newSize ->
            let
                width =
                    toFloat newSize.width

                height =
                    toFloat newSize.height

                newPerspective =
                    Matrix4.makePerspective 45 (width / height) 0.01 100
            in
                ( { model | size = newSize, perspectiveMatrix = newPerspective }, Cmd.none )

        ClickedCanvas ->
            ( model, Pointer.lockPointer )

        Animate diff ->
            let
                dt =
                    diff / 200
            in
                ( model
                    |> physics dt
                , Cmd.none
                )

        MouseMove movement ->
            let
                ( newRotation, newTilt ) =
                    applyMouseMovement movement ( model.rotation, model.tilt )
            in
                ( { model | rotation = newRotation, tilt = newTilt }, Cmd.none )

        KeyChange newState keyCode ->
            let
                newKeys =
                    applyKeyChange newState keyCode model.keys

                newModel =
                    applyKeys newKeys model
            in
                ( { newModel | keys = newKeys }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        size =
            model.size

        xAxis =
            cos model.rotation

        zAxis =
            sin model.rotation

        viewMatrix =
            model.perspectiveMatrix
                |> Matrix4.rotate model.rotation (vec3 0 1 0)
                |> Matrix4.rotate model.tilt (vec3 xAxis 0 zAxis)
                |> Matrix4.translate (Vector3.negate >> Vector3.scale 2 <| model.position)

        log =
            Debug.log "Position" model.position

        makePositions n =
            if n == 0 then
                [ vec3 0 0 0 ]
            else
                vec3 (n - 1) 0 0 :: makePositions (n - 1)
    in
        GL.toHtml
            [ width size.width, height size.height, onClick ClickedCanvas, onMouseMove MouseMove ]
            (List.map (Ground.groundEntity viewMatrix) (makePositions 4))


getInitialWindowSize : Cmd Msg
getInitialWindowSize =
    Task.perform Resize Window.size



-- Input stuff


type alias MouseMovement =
    ( Float, Float )


type alias Keys =
    { forward : Bool
    , back : Bool
    , left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    }


onMouseMove : (MouseMovement -> Msg) -> Attribute Msg
onMouseMove message =
    on "mousemove" (Decode.map message decodeMouseEvent)


decodeMouseEvent : Decode.Decoder MouseMovement
decodeMouseEvent =
    Decode.map2 (,)
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


applyMouseMovement : MouseMovement -> ( Float, Float ) -> ( Float, Float )
applyMouseMovement ( mvX, mvY ) ( rotation, tilt ) =
    let
        newRotation =
            rotation + (mvX / 400)

        newTilt =
            clamp -1 1 <| tilt + (mvY / 400)

        wrap n around =
            if abs n > around then
                abs n - around
            else
                n
    in
        ( wrap newRotation (pi * 2), newTilt )


applyKeyChange : Bool -> Keyboard.KeyCode -> Keys -> Keys
applyKeyChange newState keyCode keys =
    case keyCode of
        87 ->
            { keys | forward = newState }

        83 ->
            { keys | back = newState }

        65 ->
            { keys | left = newState }

        68 ->
            { keys | right = newState }

        32 ->
            { keys | up = newState }

        16 ->
            { keys | down = newState }

        _ ->
            keys


applyKeys : Keys -> Model -> Model
applyKeys { forward, back, left, right, up, down } model =
    let
        direction a b =
            case ( a, b ) of
                ( True, False ) ->
                    -1

                ( False, True ) ->
                    1

                _ ->
                    0
    in
        { model
            | velocity =
                vec3
                    (direction left right)
                    (direction down up)
                    (direction forward back)
        }



-- Animation and physics stuff


rotateVector3Y : Float -> Vec3 -> Vec3
rotateVector3Y rotateBy vector =
    let
        x =
            Vector3.getX vector

        z =
            Vector3.getZ vector

        rot =
            -rotateBy
    in
        vector
            |> Vector3.setX (x * cos rot + z * sin rot)
            |> Vector3.setZ (-x * sin rot + z * cos rot)


physics : Float -> Model -> Model
physics dt model =
    { model
        | position =
            Vector3.add model.position <|
                Vector3.scale dt <|
                    rotateVector3Y model.rotation model.velocity
    }