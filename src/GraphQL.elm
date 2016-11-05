module GraphQL
    exposing
        ( query
        , mutation
        , apply
        , maybeEncode
        )

{-| This library provides support functions used by
    [elm-graphql](https://github.com/jahewson/elm-graphql), the GraphQL code generator for Elm.

# Helper functions
@docs query, mutation, apply, maybeEncode

-}

import Task exposing (Task)
import Json.Decode exposing (..)
import Json.Encode
import Http


type alias Header =
    ( String, String )


{-| Executes a GraphQL query.
-}
query : String -> String -> List Header -> String -> String -> Json.Encode.Value -> Decoder a -> Task Http.Error a
query method url customHeaders query operation variables decoder =
    fetch method url customHeaders query operation variables decoder


{-| Executes a GraphQL mutation.
-}
mutation : String -> List Header -> String -> String -> Json.Encode.Value -> Decoder a -> Task Http.Error a
mutation url customHeaders query operation variables decoder =
    fetch "POST" url customHeaders query operation variables decoder


fetch : String -> String -> List Header -> String -> String -> Json.Encode.Value -> Decoder a -> Task Http.Error a
fetch verb url customHeaders query operation variables decoder =
    let
        request =
            (case verb of
                "GET" ->
                    buildRequestWithQuery verb url customHeaders query operation variables

                _ ->
                    buildRequestWithBody verb url customHeaders query operation variables
            )
    in
        Http.fromJson (queryResult decoder) (Http.send Http.defaultSettings request)


buildRequestWithQuery : String -> String -> List Header -> String -> String -> Json.Encode.Value -> Http.Request
buildRequestWithQuery verb url customHeaders query operation variables =
    let
        params =
            [ ( "query", query )
            , ( "operationName", operation )
            , ( "variables", (Json.Encode.encode 0 variables) )
            ]
    in
        { verb = verb
        , headers = [ ( "Accept", "application/json" ) ] ++ customHeaders
        , url = Http.url url params
        , body = Http.empty
        }


buildRequestWithBody : String -> String -> List Header -> String -> String -> Json.Encode.Value -> Http.Request
buildRequestWithBody verb url customHeaders query operation variables =
    let
        params =
            Json.Encode.object
                [ ( "query", Json.Encode.string query )
                , ( "operationName", Json.Encode.string operation )
                , ( "variables", variables )
                ]
    in
        { verb = verb
        , headers =
            [ ( "Accept", "application/json" )
            , ( "Content-Type", "application/json" )
            ]
                ++ customHeaders
        , url = Http.url url []
        , body = Http.string <| Json.Encode.encode 0 params
        }


queryResult : Decoder a -> Decoder a
queryResult decoder =
    oneOf
        [ at [ "data" ] decoder
        , fail "Expected 'data' field"
          -- todo: report failure reason from server
        ]


{-| Combines two object decoders.
-}
apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply func value =
    object2 (<|) func value


{-| Encodes a `Maybe` as JSON, using `null` for `Nothing`.
-}
maybeEncode : (a -> Value) -> Maybe a -> Value
maybeEncode e v =
    case v of
        Nothing ->
            Json.Encode.null

        Just a ->
            e a
