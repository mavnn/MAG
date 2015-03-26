module MAG.Server.EventStore

open MAG
open EventStore.ClientAPI
open Chiron
open Chessie.ErrorHandling

let store = 
    (ConnectionSettings.Create()
        .UseConsoleLogger()
        .Build(),
     System.Net.IPEndPoint(System.Net.IPAddress.Parse "192.168.33.10", 1113))
    |> EventStoreConnection.Create

store.ConnectAsync().Wait()

let inline encode event =
    Json.serialize event
    |> Json.format 
    |> System.Text.Encoding.UTF8.GetBytes

let inline decode (bytes : byte []) =
    bytes
    |> System.Text.Encoding.UTF8.GetString
    |> fun s ->
        printfn "%s" s
        s
    |> Json.parse
    |> Json.deserialize

type State = {
        Generation : int
        Game : Game
    }

let eventFold result event =
    result |> bind (fun state ->
        GameEvents.processEvent event state.Game
        |> lift (fun g -> { Generation = state.Generation + 1; Game = g }))

let start = trial { return { Generation = -1; Game = Nothing } }

let foldEvents2 state events =
    Seq.fold eventFold state events

let readEvents gid : GameEvent seq =
    let rec inner i =
        seq {
            let slice = store.ReadStreamEventsForwardAsync(sprintf "game-%s" gid, i, 10, true).Result
            let events =
                slice.Events
                |> Seq.map (fun e -> e.Event.Data)
            yield!
                events
                |> Seq.map decode
            if not slice.IsEndOfStream then
                yield! inner (slice.NextEventNumber)
        }
    inner 0

let reloaded gid =
    readEvents gid
    |> foldEvents2 start
