module MAG.Server.EventStore

open System
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

let inline refresh currentState eventNumber fold streamName =
    let rec inner currentState eventNumber fold streamName =
        async {
            let! slice =
                store.ReadStreamEventsForwardAsync(streamName, eventNumber, 10, true)
                |> Async.AwaitTask
            let nextState =
                slice.Events
                |> Seq.map (fun e -> e.Event.Data |> decode)
                |> Seq.fold fold currentState
            if slice.IsEndOfStream then
                return (nextState, slice.LastEventNumber)
            else
                return! inner nextState slice.NextEventNumber fold streamName
        }
    inner currentState eventNumber fold streamName

let inline persist expected eventType events streamName =
    events
    |> Seq.map (fun e -> EventData(Guid.NewGuid(), eventType, true, encode e, [||]))
    |> fun eventDatas -> store.AppendToStreamAsync(streamName, expected, eventDatas)
    |> Async.AwaitTask
    |> Async.map (fun wr -> wr.NextExpectedVersion)
