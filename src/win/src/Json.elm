module Json exposing
    ( Value
    , decoder
    , encode
    , fromString
    , toInt
    , toString
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type Value
    = True
    | False
    | Null
    | Number Float
    | String String.String


fromString : String -> Value
fromString str =
    if str == "true" then
        True

    else if str == "false" then
        False

    else if str == "NULL" then
        Null

    else
        case String.toFloat str of
            Just n ->
                Number n

            Nothing ->
                if String.left 1 str == "\"" && String.right 1 str == "\"" then
                    String <| String.slice 1 -1 str

                else
                    String str


toString : Value -> String
toString jv =
    case jv of
        String str ->
            str

        True ->
            "true"

        False ->
            "false"

        Number n ->
            String.fromFloat n

        Null ->
            "null"


toInt : Value -> Maybe Int
toInt jv =
    case jv of
        Number n ->
            Just <| truncate n

        _ ->
            Nothing


decoder : Decoder Value
decoder =
    D.oneOf
        [ D.map
            (\bool ->
                if bool then
                    True

                else
                    False
            )
            D.bool
        , D.map Number D.float
        , D.null Null
        , D.map String D.string
        ]


encode : Value -> E.Value
encode jv =
    case jv of
        True ->
            E.bool Basics.True

        False ->
            E.bool Basics.False

        Number n ->
            E.float n

        String str ->
            E.string str

        Null ->
            E.null

