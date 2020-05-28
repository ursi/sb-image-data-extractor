module SupPort exposing (in_, out)

import Dict
import Json.Decode as D exposing (Decoder, Value, decodeValue)
import Json.Encode as E


out : (Value -> Cmd msg) -> String -> Value -> Cmd msg
out outPort =
    \msgName value ->
        outPort <|
            E.object
                [ ( "msg", E.string msgName )
                , ( "data", value )
                ]


in_ :
    ((Value -> msg) -> Sub msg)
    -> (D.Error -> portMsg)
    -> List ( String, Decoder portMsg )
    -> (portMsg -> msg)
    -> Sub msg
in_ inPort error msgDataToMsgList =
    let
        msgDataToMsgDict =
            Dict.fromList msgDataToMsgList
    in
    \msgDataToMsg ->
        inPort <|
            \msgData ->
                case decodeValue (D.field "msg" D.string) msgData of
                    Ok msgName ->
                        let
                            decoder =
                                Maybe.withDefault
                                    (D.map error <| D.fail <| "'" ++ msgName ++ "'" ++ " is not a valid message.")
                                    (Dict.get msgName msgDataToMsgDict)
                        in
                        case decodeValue (D.field "data" decoder) msgData of
                            Ok value ->
                                msgDataToMsg value

                            Err decoderError ->
                                msgDataToMsg <| error decoderError

                    Err decoderError ->
                        msgDataToMsg <| error decoderError

