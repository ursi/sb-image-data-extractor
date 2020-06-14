module Main exposing (..)

import Browser exposing (Document)
import Debug exposing (log)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D exposing (Decoder)
import Json.Encode as En exposing (Value)
import Node.Fs as Fs
import Node.Path as Path
import Ports
import Protobuf exposing (Image, Pb)
import Task exposing (Task)


todo =
    Debug.todo ""


main : Program String Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    String


init : String -> ( Model, Cmd Msg )
init dirname =
    ( dirname, Cmd.none )



-- UPDATE


type Msg
    = DataReceived String
    | JsonReceived Pb
    | PortError D.Error
    | TransformFiles
    | FilesTransformed
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FilesTransformed ->
            let
                _ =
                    Debug.log "done" ()
            in
            ( model, Cmd.none )

        TransformFiles ->
            ( model
            , let
                basePath =
                    Path.join [ model, "bpb" ]

                path file =
                    Path.join [ basePath, file ]

                jsonPath =
                    Path.join [ model, "json" ]
              in
              Fs.exists jsonPath
                |> Task.andThen
                    (\exists ->
                        if not exists then
                            Fs.mkdir jsonPath

                        else
                            Task.succeed ()
                    )
                |> Task.andThen
                    (\_ ->
                        basePath
                            |> Fs.readdir
                            |> Task.andThen
                                (List.map
                                    (\file ->
                                        file
                                            |> path
                                            |> Fs.readFile
                                            |> Task.map
                                                (\a ->
                                                    let
                                                        _ =
                                                            Debug.log "file" file
                                                    in
                                                    En.encode 0 <| Protobuf.parse a
                                                )
                                            |> Task.andThen
                                                (Fs.writeFile <|
                                                    Path.join
                                                        [ jsonPath
                                                        , String.replace ".txt" ".json" file
                                                        ]
                                                )
                                    )
                                    >> Task.sequence
                                )
                    )
                |> Task.attempt (\_ -> NoOp)
            )

        JsonReceived body ->
            -- ( model, logValue <| encodeBody body )
            -- ( model, logValue <| E.list E.string <| getVids body )
            -- ( model, logValue <| E.array encodeImage <| getNakedImages body )
            -- ( model, logValue <| E.array encodeImage <| getImages body )
            ( model
            , Cmd.batch
                [ Ports.logValue <|
                    En.list Protobuf.encodeImage <|
                        Protobuf.findImages [ "auraduskwing" ]
                            body
                , Ports.logValue <|
                    En.list En.string <|
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



-- VIEW


view : Model -> Document Msg
view model =
    { title = ""
    , body = [ H.button [ E.onClick TransformFiles ] [ H.text "transform files" ] ]
    }
