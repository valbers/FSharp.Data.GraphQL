namespace FSharp.Data.GraphQL.Shared.Websockets

type InvalidWebsocketMessageException (explanation : string) =
    inherit System.Exception (explanation)