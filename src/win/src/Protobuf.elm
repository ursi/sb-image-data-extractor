module Protobuf exposing
    ( Image
    , Pb
    , decode
    , encode
    , encodeImage
    , findImages
    , getImages
    , getVids
    , parse
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Json
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Parser as P exposing ((|.), (|=), Parser, Step(..))
import Set exposing (Set)


type alias Pb =
    Body


type alias Body =
    { properties : Dict String Json.Value
    , objects : List Object
    }


type Object
    = Object String Body


type alias Image =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    , sheet : Int
    , vids : Set String
    }


encode : Body -> Value
encode =
    encodeBody


parse : String -> Value
parse =
    parseData


decode : Decoder Body
decode =
    decodeBody


{-| find all images corresponding to a list of vids
-}
findImages : List String -> Body -> List Image
findImages objects body =
    Array.foldr
        (\image images ->
            if
                image.vids
                    |> Set.toList
                    |> overlaps objects
            then
                image :: images

            else
                images
        )
        []
        (getImages body)


overlaps : List a -> List a -> Bool
overlaps l1 l2 =
    case l1 of
        first :: rest ->
            if List.any ((==) first) l2 then
                True

            else
                overlaps rest l2

        [] ->
            False


getImages : Body -> Array Image
getImages body =
    body.objects
        |> List.filter
            (\(Object head _) ->
                List.any ((==) head)
                    [ "animatedImageDesc"
                    , "imageVidDesc"
                    ]
            )
        |> List.foldl
            (\(Object head body2) images ->
                case Dict.get "vid" body2.properties of
                    Just vidValue ->
                        let
                            vid =
                                Json.toString vidValue
                        in
                        if head == "animatedImageDesc" then
                            getImagesHelper
                                vid
                                body2.objects
                                images

                        else
                            addVid vid body2.properties images

                    Nothing ->
                        Debug.log "never called" images
            )
            (getNakedImages body)


getImagesHelper : String -> List Object -> Array Image -> Array Image
getImagesHelper vid objects =
    List.foldl
        (\(Object head body) images ->
            if head == "element" then
                addVid vid body.properties images

            else
                getImagesHelper vid body.objects images
        )
        >> (|>) objects


addVid : String -> Dict String Json.Value -> Array Image -> Array Image
addVid vid properties images =
    properties
        |> Dict.get "uiid"
        |> Maybe.andThen Json.toInt
        |> Maybe.andThen
            (\uiid ->
                images
                    |> Array.get uiid
                    |> Maybe.map
                        (\image ->
                            Array.set
                                uiid
                                { image | vids = Set.insert vid image.vids }
                                images
                        )
            )
        |> Maybe.withDefault images


encodeImage : Image -> Value
encodeImage image =
    E.object
        [ ( "x", E.int image.x )
        , ( "y", E.int image.y )
        , ( "w", E.int image.w )
        , ( "h", E.int image.h )
        , ( "sheet", E.int image.sheet )
        , ( "vids", E.set E.string image.vids )
        ]


getNakedImages : Body -> Array Image
getNakedImages { objects } =
    objects
        |> List.filter (\(Object head _) -> head == "sheetDef")
        |> List.indexedMap
            (\i (Object _ sheetDef) ->
                sheetDef.objects
                    |> List.map
                        (\(Object _ imageDef) ->
                            Maybe.map4 (\x y w h -> Image x y w h i Set.empty)
                                (getAndToInt "x" imageDef.properties)
                                (getAndToInt "y" imageDef.properties)
                                (getAndToInt "w" imageDef.properties)
                                (getAndToInt "h" imageDef.properties)
                        )
                    |> List.filterMap identity
            )
        |> List.concat
        |> Array.fromList


getAndToInt : String -> Dict String Json.Value -> Maybe Int
getAndToInt =
    Dict.get >> (<<) (Maybe.andThen Json.toInt)


getVids : Body -> List String
getVids { properties, objects } =
    objects
        |> List.map (\(Object _ body) -> getVids body)
        |> List.concat
        |> (properties
                |> Dict.get "vid"
                |> Maybe.map Json.toString
                |> Maybe.map (::)
                |> Maybe.withDefault identity
           )


parseData : String -> Value
parseData data =
    case P.run parseBody data of
        Ok output ->
            encodeBody output

        Err error ->
            let
                _ =
                    Debug.log "error" error

                _ =
                    Debug.log "data" data
            in
            E.string "error"


encodeBody : Body -> Value
encodeBody { properties, objects } =
    E.object
        [ ( "properties", E.dict identity Json.encode properties )
        , ( "objects", E.list encodeObject objects )
        ]


encodeObject : Object -> Value
encodeObject (Object head body) =
    E.object
        [ ( "head", E.string head )
        , ( "body", encodeBody body )
        ]


parseObject : Parser Object
parseObject =
    P.succeed
        (\head body ->
            Object
                (String.trim head)
                body
        )
        |= P.getChompedString (P.chompWhile notBraceOrColon)
        |. P.symbol "{"
        |= parseBody
        |. P.symbol "}"


parseBody : Parser Body
parseBody =
    P.loop
        (Body Dict.empty [])
        (\body ->
            P.oneOf
                [ P.backtrackable <|
                    P.succeed
                        (\p v ->
                            Loop <|
                                { body
                                    | properties =
                                        Dict.insert
                                            (String.trim p)
                                            (Json.fromString <| String.trim v)
                                            body.properties
                                }
                        )
                        |= P.getChompedString (P.chompWhile notBraceOrColon)
                        |. P.symbol ":"
                        |= P.getChompedString (P.chompUntilEndOr "\n")
                , P.backtrackable <|
                    P.succeed
                        (\object ->
                            Loop <| { body | objects = object :: body.objects }
                        )
                        |= parseObject
                , P.succeed
                    (\str offset ->
                        let
                            nextChar =
                                String.slice offset (offset + 1) str
                        in
                        if nextChar == "}" || String.isEmpty nextChar then
                            { body | objects = List.reverse body.objects }
                                |> Done
                                |> P.succeed

                        else
                            P.problem "something went wrong"
                    )
                    |. P.spaces
                    |= P.getSource
                    |= P.getOffset
                    |> P.andThen identity
                ]
        )


notBraceOrColon : Char -> Bool
notBraceOrColon c =
    c /= '{' && c /= '}' && c /= ':'


decodeBody : Decoder Body
decodeBody =
    D.map2 Body
        (D.field "properties" <| D.dict Json.decoder)
        (D.field "objects" <| D.list <| D.lazy (\_ -> decodeObject))


decodeObject : Decoder Object
decodeObject =
    D.map2 Object
        (D.field "head" D.string)
        (D.field "body" decodeBody)
