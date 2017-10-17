module Network exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard exposing (KeyCode)
import Result
import WebSocket


send : PlayerMessage -> Cmd msg
send =
    WebSocket.send "ws://127.0.0.1:9160" << encodePlayerMessage


connect : String -> Cmd msg
connect =
    send << ConnectionRequest


type ServerMessage
    = Connected String



-- TODO: Refactor for the looks and (efficiency?)


parseServerMessage : String -> Maybe ServerMessage
parseServerMessage str =
    let
        typeDecoder =
            Decode.field "type" Decode.string

        resultMessageType =
            Decode.decodeString typeDecoder str

        maybeDataDecoder =
            case resultMessageType of
                Ok msgType ->
                    mapTypeToDecoder msgType

                _ ->
                    Nothing
    in
        case maybeDataDecoder of
            Just decoder ->
                case Decode.decodeString decoder str of
                    Ok msg ->
                        Just msg

                    Err _ ->
                        Nothing

            Nothing ->
                Nothing


mapTypeToDecoder : String -> Maybe (Decode.Decoder ServerMessage)
mapTypeToDecoder messageType =
    case messageType of
        "connected" ->
            Just <| parse1 Connected Decode.string

        _ ->
            Nothing


parse1 : (a -> ServerMessage) -> Decode.Decoder a -> Decode.Decoder ServerMessage
parse1 msg decoder =
    Decode.map msg (Decode.field "0" decoder)


type PlayerMessage
    = ConnectionRequest String
    | KeyChange KeyCode Bool


encodePlayerMessage : PlayerMessage -> String
encodePlayerMessage msg =
    let
        ( msgType, args ) =
            case msg of
                ConnectionRequest username ->
                    ( "connection_request", [ Encode.string username ] )

                KeyChange keyCode newState ->
                    ( "keychange", [ Encode.int keyCode, Encode.bool newState ] )

        mapArgs n args =
            case args of
                [] ->
                    []

                arg :: rest ->
                    ( toString n, arg ) :: mapArgs (n + 1) rest
    in
        Encode.encode 0 << Encode.object <| ( "type", Encode.string msgType ) :: mapArgs 0 args
