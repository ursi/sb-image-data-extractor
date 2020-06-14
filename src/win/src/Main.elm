module Main exposing (..)

import Browser exposing (Document)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D exposing (Decoder)
import Json.Encode as En exposing (Value)
import Node.Console as Console exposing (log)
import Node.Fs as Fs
import Node.Path as Path
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
    = TransformFiles
    | TaskFinished
    | TrimFat
    | JsonReceived (Result D.Error Pb)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JsonReceived result ->
            case result of
                Ok body ->
                    let
                        _ =
                            log <|
                                En.array Protobuf.encodeImage <|
                                    Protobuf.getImages body
                    in
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        TrimFat ->
            ( model
            , ensureDir (imagePath model)
                |> Task.andThen
                    (\_ ->
                        jsonPath model
                            |> Fs.readdir
                            |> Task.andThen
                                (List.map
                                    (\file ->
                                        Path.join [ jsonPath model, file ]
                                            |> Fs.readFile
                                            |> Task.andThen
                                                (\json ->
                                                    case D.decodeString Protobuf.decode json of
                                                        Ok value ->
                                                            Task.succeed value

                                                        Err error ->
                                                            Task.fail error
                                                )
                                            |> Task.andThen
                                                (Protobuf.getImages
                                                    >> En.array Protobuf.encodeImage
                                                    >> En.encode 0
                                                    >> Fs.writeFile (Path.join [ model, "images", file ])
                                                )
                                    )
                                    >> Task.sequence
                                )
                    )
                |> Task.attempt (\_ -> TaskFinished)
            )

        TaskFinished ->
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
              in
              ensureDir (jsonPath model)
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
                                                        [ jsonPath model
                                                        , String.replace ".txt" ".json" file
                                                        ]
                                                )
                                    )
                                    >> Task.sequence
                                )
                    )
                |> Task.attempt (\_ -> TaskFinished)
            )

        NoOp ->
            ( model, Cmd.none )


jsonPath : Model -> String
jsonPath dirname =
    Path.join [ dirname, "json" ]


imagePath : Model -> String
imagePath dirname =
    Path.join [ dirname, "images" ]


ensureDir : String -> Task D.Error ()
ensureDir path =
    Fs.exists path
        |> Task.andThen
            (\exists ->
                if not exists then
                    Fs.mkdir path

                else
                    Task.succeed ()
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = ""
    , body =
        [ makeButton "transfrom files" TransformFiles
        , makeButton "trim fat" TrimFat
        ]
    }


makeButton : String -> Msg -> Html Msg
makeButton text msg =
    H.div [] [ H.button [ E.onClick msg ] [ H.text text ] ]
