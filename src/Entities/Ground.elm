module Entities.Ground exposing (groundEntity)

import WebGL as GL exposing (..)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)


type alias Vertex =
    { vertexPosition : Vec3
    }


type alias Uniforms =
    { viewMatrix : Mat4
    , position : Vec3
    }


groundEntity : Mat4 -> Vec3 -> Entity
groundEntity viewMatrix position =
    GL.entity
        vertexShader
        fragmentShader
        (GL.triangles groundMesh)
        (Uniforms viewMatrix position)


groundMesh : List ( Vertex, Vertex, Vertex )
groundMesh =
    [ ( Vertex (vec3 -1 -1 1)
      , Vertex (vec3 -1 -1 -1)
      , Vertex (vec3 1 -1 1)
      )
    , ( Vertex (vec3 1 -1 1)
      , Vertex (vec3 -1 -1 -1)
      , Vertex (vec3 1 -1 -1)
      )
    ]


vertexShader : Shader Vertex Uniforms { vColor : Vec3 }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 vertexPosition;
        uniform mat4 viewMatrix;
        uniform vec3 position;
        varying vec3 vColor;

        void main() {
          vec3 finalPosition = vertexPosition + position * vec3(2);

          gl_Position = viewMatrix * vec4(finalPosition, 1);
          vColor = position / vec3(4);
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
