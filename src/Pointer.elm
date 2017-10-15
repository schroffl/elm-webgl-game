port module Pointer exposing (lockPointer)


port lockPointerPort : () -> Cmd msg


lockPointer : Cmd msg
lockPointer =
    lockPointerPort ()
