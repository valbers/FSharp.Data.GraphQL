// Uncomment those to use build script client assembly
//#r "../../bin/FSharp.Data.GraphQL.Client/ne47/FSharp.Data.GraphQL.Client.dll"
//#r "../../bin/FSharp.Data.GraphQL.Shared/ne47/FSharp.Data.GraphQL.Shared.dll"

// Uncomment those to use dotnet build command for the client assembly
#r "../../src/FSharp.Data.GraphQL.Shared/bin/Debug/net47/FSharp.Data.GraphQL.Shared.dll"
#r "../../src/FSharp.Data.GraphQL.Client/bin/Debug/net47/FSharp.Data.GraphQL.Client.dll"

open FSharp.Data.GraphQL

// The URL here is for design time purposes.
// It connects to the server to be able to map its schema.
type MyProvider = GraphQLProvider<"http://localhost:8084">

// Once mapped, all custom types of the schema (types that are not scalar types)
// will be mapped into CLR types. You can create those types by filling each of its
// properties into the constructor.
let customBall = MyProvider.Types.Ball("Circular", "Ball")

printfn "%A\n" customBall

// Once created the provider and the schema is successfully mapped,
// We can start doing queries. You can optionally specify an runtime URL for the server.
let runtimeUrl = "http://localhost:8084"

// A context exists for reusing the same schema against different servers by filling an
// runtime URL if needed. If not, the context will use the same static URL used for the
// provider definition.
let ctx = MyProvider.GetContext(runtimeUrl)

// If your server requires custom HTTP headers (for example, authentication headers),
// you can specify them as a (string * string) seq.
let customHttpHeaders : (string * string) seq = upcast [||]

// The operation method can be used to make queries, mutations, and subscriptions.
// Although subscription operations can be created, the client provider still
// does not work with web sockets - only the immediate response will be known.
let operation = 
    ctx.Operation<"""query q {
      hero (id: "1000") {
        name
        appearsIn
        homePlanet
        friends {
          ... on Human {
            name
            homePlanet
          }
          ... on Droid {
            name
            primaryFunction
          }
        }
      }
    }""">(customHttpHeaders)

printfn "Headers: %A" operation.CustomHttpHeaders
printfn "Server: %s\n" operation.ServerUrl

// To run an operation, you just need to call the Run method.
let result = operation.Run()

// If the operation were successfull, result data will be on the Data property.
let data = result.Data

// Query result objects have pretty-printing and structural equality.
printfn "Data: %A\n" data

let hero = data.Hero.Value

// GraphQL enum types are essentially strings, and here they are mapped to
// custom objects with string values inside. Each enum value does have an static
// instance of itself, and does have structural equality against other enum types.
// However, they are not classic CLR enum types.
if hero.AppearsIn |> Array.exists (fun x -> x = MyProvider.Types.Episode.Empire)
then printfn "Hero appears in Empire episode!\n"
else printfn "Hero does not appear in Empire episode!\n"

let friends = hero.Friends |> Array.choose id

// When we have interfaces or union types in the GraphQL schema, they are mapped as
// Inherited objects in the client. However, as the type provider uses erased types,
// we can't pattern match them by classic pattern matching. Instead, we use the following
// methods of types generated by GraphQL union or interface types:

// This will produce an error. Not all friends are droids!
//let thisWillProduceAnError = friends |> Array.map (fun x -> x.AsDroid())

// We can easily filter friends by using "TryAs" methods.
let humanFriends = friends |> Array.choose (fun x -> x.TryAsHuman())
let droidFriends = friends |> Array.choose (fun x -> x.TryAsDroid())

// We can also use "Is" version methods to do some custom matching.
let humanFriendsCount = friends |> Array.map (fun x -> if x.IsHuman() then 1 else 0) |> Array.reduce (+)
let droidFriendsCount = friends |> Array.map (fun x -> if x.IsDroid() then 1 else 0) |> Array.reduce (+)

printfn "Hero friends (%i): %A\n" friends.Length friends
printfn "Hero human friends (%i): %A\n" humanFriendsCount humanFriends
printfn "Hero droid friends (%i): %A\n" droidFriendsCount droidFriends