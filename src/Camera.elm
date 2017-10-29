module Camera exposing (..)

import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Window


type alias Orientation =
    ( Float, Float )


type alias MouseMovement =
    ( Float, Float )


type alias Camera =
    { position : Vec3
    , orientation : Orientation
    , perspectiveMatrix : Mat4
    }


initialize : Camera
initialize =
    { position = vec3 0 2 0
    , orientation = ( 0, 0 )
    , perspectiveMatrix = Matrix4.identity
    }


makeViewMatrix : Camera -> Mat4
makeViewMatrix cam =
    let
        ( rotation, tilt ) =
            cam.orientation

        xAxis =
            cos rotation

        zAxis =
            sin rotation
    in
        cam.perspectiveMatrix
            |> Matrix4.rotate rotation (vec3 0 1 0)
            |> Matrix4.rotate tilt (vec3 xAxis 0 zAxis)
            |> Matrix4.translate (Vector3.scale 2 << Vector3.negate <| cam.position)


updatePerspective : Window.Size -> Camera -> Camera
updatePerspective size cam =
    let
        width =
            toFloat size.width

        height =
            toFloat size.height

        ratio =
            width / height
    in
        { cam
            | perspectiveMatrix =
                Matrix4.makePerspective 45 ratio 0.01 100
        }


applyMouseMovement : MouseMovement -> Camera -> Camera
applyMouseMovement ( mvX, mvY ) cam =
    let
        ( rotation, tilt ) =
            cam.orientation

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
        { cam | orientation = ( newRotation, newTilt ) }
