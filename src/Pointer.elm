port module Pointer exposing (lockPointer, pointerLockChange)


port pointerLockChangePort : (Bool -> msg) -> Sub msg


pointerLockChange : (Bool -> msg) -> Sub msg
pointerLockChange message =
    pointerLockChangePort message


port lockPointerPort : () -> Cmd msg


lockPointer : Cmd msg
lockPointer =
    lockPointerPort ()
