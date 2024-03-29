module RemoteData exposing (..)

import Http


type RemoteData a
    = Loading
    | Failure Http.Error
    | Success a


succeed : a -> RemoteData a
succeed data =
    Success data


fail : Http.Error -> RemoteData a
fail error =
    Failure error


withDefault : a -> RemoteData a -> a
withDefault default data =
    case data of
        Success value ->
            value

        _ ->
            default


fromResult : Result Http.Error a -> RemoteData a
fromResult result =
    case result of
        Ok value ->
            Success value

        Err err ->
            Failure err


map : (a -> b) -> RemoteData a -> RemoteData b
map f data =
    case data of
        Success value ->
            Success (f value)

        Loading ->
            Loading

        Failure err ->
            Failure err
