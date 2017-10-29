module Entities.Ground exposing (groundEntity)

import WebGL as GL exposing (..)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)


type alias Vertex =
    { vertexPosition : Vec3
    }


type alias Uniforms =
    { viewMatrix : Mat4
    , size : Float
    }


groundEntity : Mat4 -> Float -> Entity
groundEntity viewMatrix size =
    GL.entity
        vertexShader
        fragmentShader
        groundMesh
        (Uniforms viewMatrix size)


groundMesh : Mesh Vertex
groundMesh =
    GL.triangles
        [ ( Vertex (vec3 -1 0 1)
          , Vertex (vec3 -1 0 -1)
          , Vertex (vec3 1 0 1)
          )
        , ( Vertex (vec3 1 0 1)
          , Vertex (vec3 -1 0 -1)
          , Vertex (vec3 1 0 -1)
          )
        ]


vertexShader : Shader Vertex Uniforms { vColor : Vec3 }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 vertexPosition;
        uniform mat4 viewMatrix;
        uniform float size;
        varying vec3 vColor;

        void main() {
          vec3 finalPosition = vertexPosition * vec3(size, 0, size);

          gl_Position = viewMatrix * vec4(finalPosition, 1);
          vColor = finalPosition / vec3(size, 0, size);
        }
    |]


fragmentShader : Shader {} Uniforms { vColor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 vColor;

        void main() {
            gl_FragColor = vec4(vColor, 1);
            // gl_FragColor = vec4(0.3, 0.3, 0.7, 1);
        }

    |]
