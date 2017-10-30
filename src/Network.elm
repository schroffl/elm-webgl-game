module Network exposing (send, connect, parseServerMessage, PlayerMessage(..), ServerMessage(..))

import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard exposing (KeyCode)
import Result
import WebSocket


send : String -> PlayerMessage -> Cmd msg
send url msg =
    WebSocket.send url <| encodePlayerMessage msg


connect : String -> String -> Cmd msg
connect url username =
    send url <| ConnectionRequest username


type ServerMessage
    = Connected


parseServerMessage : String -> Result String ServerMessage
parseServerMessage str =
    let
        messageType =
            Decode.decodeString (Decode.field "type" Decode.string) str

        dataDecoder =
            messageType |> Result.andThen mapTypeToDecoder
    in
        dataDecoder |> Result.andThen (flip Decode.decodeString str)


mapTypeToDecoder : String -> Result String (Decode.Decoder ServerMessage)
mapTypeToDecoder messageType =
    case messageType of
        "connected" ->
            Ok <| Decode.succeed Connected

        _ ->
            Err <| "Unknown message type: " ++ messageType


type PlayerMessage
    = ConnectionRequest String


encodePlayerMessage : PlayerMessage -> String
encodePlayerMessage msg =
    let
        ( msgType, args ) =
            case msg of
                ConnectionRequest username ->
                    ( "connection_request", [ Encode.string username ] )

        mapArgs n args =
            case args of
                [] ->
                    []

                arg :: rest ->
                    ( toString n, arg ) :: mapArgs (n + 1) rest
    in
        Encode.encode 0 << Encode.object <| ( "type", Encode.string msgType ) :: mapArgs 0 args
