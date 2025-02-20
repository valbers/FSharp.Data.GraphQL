module FSharp.Data.GraphQL.Samples.ChatApp.Program

open Giraffe
open FSharp.Data.GraphQL.Server.AspNetCore
open FSharp.Data.GraphQL.Server.AspNetCore.Giraffe
open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

let rootFactory (ctx : HttpContext) : Root = { RequestId = ctx.TraceIdentifier }

let errorHandler (ex : Exception) (log : ILogger) =
    log.LogError (EventId (), ex, "An unhandled exception has occurred while executing this request.")
    clearResponse >=> setStatusCode 500

[<EntryPoint>]
let main args =

    let builder = WebApplication.CreateBuilder (args)
    builder.Services
        .AddGiraffe()
        .AddGraphQL<Root> (Schema.executor, rootFactory)
    |> ignore

    let app = builder.Build ()

    if app.Environment.IsDevelopment () then
        app.UseGraphQLAltair "/altair" |> ignore
        app.UseGraphQLGraphiQL "/graphiql" |> ignore
        app.UseGraphQLVoyager "/voyager" |> ignore
        app.UseRouting () |> ignore
        app.UseEndpoints (fun endpoints -> endpoints.MapNitroApp (PathString "/nitro") |> ignore)
        |> ignore

    app
        .UseGiraffeErrorHandler(errorHandler)
        .UseWebSockets()
        .UseWebSocketsForGraphQL<Root>()
        .UseGiraffe (HttpHandlers.graphQL<Root>)

    app.Run ()

    0 // Exit code
