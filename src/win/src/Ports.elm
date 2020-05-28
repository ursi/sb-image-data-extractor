port module Ports exposing
    ( Msg(..)
    , in_
    , logValue
    , toJson
    )

import Json.Decode as D exposing (Value)
import Json.Encode as E
import Protobuf exposing (Pb)
import SupPort


type Msg
    = Error D.Error
    | DataReceived String
    | JsonReceived Pb


toJson : Value -> Cmd msg
toJson =
    out "toJson"


logValue : Value -> Cmd msg
logValue =
    out "logValue"


out : String -> Value -> Cmd msg
out =
    SupPort.out portsOut


in_ : (Msg -> msg) -> Sub msg
in_ =
    SupPort.in_
        portsIn
        Error
        [ ( "DataReceived", D.map DataReceived D.string )
        , ( "JsonReceived", D.map JsonReceived Protobuf.decode )
        ]


port portsOut : Value -> Cmd msg


port portsIn : (Value -> msg) -> Sub msg
