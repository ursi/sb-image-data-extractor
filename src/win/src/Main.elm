module Main exposing (..)

import Debug exposing (log)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Ports
import Protobuf exposing (Image, Pb)


todo =
    Debug.todo ""


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )



-- UPDATE


type Msg
    = DataReceived String
    | JsonReceived Pb
    | PortError D.Error
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JsonReceived body ->
            -- ( model, logValue <| encodeBody body )
            -- ( model, logValue <| E.list E.string <| getVids body )
            -- ( model, logValue <| E.array encodeImage <| getNakedImages body )
            -- ( model, logValue <| E.array encodeImage <| getImages body )
            ( model
            , Cmd.batch
                [ Ports.logValue <|
                    E.list Protobuf.encodeImage <|
                        Protobuf.findImages [ "auraduskwing" ]
                            body
                , Ports.logValue <|
                    E.list E.string <|
                        Protobuf.getVids body
                ]
            )

        DataReceived data ->
            ( model
            , Ports.toJson <| Protobuf.parse data
            )

        PortError error ->
            let
                _ =
                    Debug.log "port error" error
            in
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.in_
        (\msg ->
            case msg of
                Ports.DataReceived str ->
                    DataReceived str

                Ports.JsonReceived pb ->
                    JsonReceived pb

                Ports.Error error ->
                    PortError error
        )

