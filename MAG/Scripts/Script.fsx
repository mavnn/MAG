(*** hide ***)

#r @"System.dll"
#r @"System.Core.dll"
#r @"System.Numerics.dll"
#r @"..\..\packages\Aether\lib\net40\Aether.dll"
#r @"..\..\packages\Chiron\lib\net40\Chiron.dll"
#r @"..\..\packages\FParsec\lib\net40-client\FParsec.dll"
#r @"..\..\packages\FParsec\lib\net40-client\FParsecCS.dll"
#r @"..\..\packages\Chessie\lib\net40\Chessie.dll"
#r @"..\..\MAG.Events\bin\Debug\MAG.Events.dll"
#r @"..\..\packages\EventStore.Client\lib\net40\EventStore.ClientAPI.dll"
#load @"..\Domain.fs"
      @"..\Card.fs"
      @"..\Player.fs"
      @"..\Action.fs"
      @"..\Decks.fs"
      @"..\Game.fs"
      @"..\Commands.fs"
      @"..\GameEvents.fs"
      @"..\GameCommands.fs"

(**
Open the required namespaces
----------------------------
*)
open MAG
open Chiron
open Chessie.ErrorHandling
open EventStore.ClientAPI

(**
Open up a connection to a local EventStore
*)
let store = 
    (ConnectionSettings.Create()
        .UseConsoleLogger()
        .Build(),
     System.Net.IPEndPoint(System.Net.IPAddress.Parse "192.168.33.10", 1113))
    |> EventStoreConnection.Create

store.ConnectAsync().Wait()

(**
Couple of helper methods to encode and decode
objects as UTF8 encoded byte arrays.
 *)
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

let inline write expected events =
    match events with
    | xs::_ ->
        let (GameId gid) = (^a : (member Id : _) xs)
        store.AppendToStreamAsync(
            sprintf "game-%A" gid,
            expected, 
            events
            |> Seq.map (fun event ->
                EventData(System.Guid.NewGuid(), "gameEvent", true, encode event, Array.empty))).Wait()
    | [] -> ()
    
let gid = GameId <| System.Guid.Parse "{26BC09F9-4585-45B3-A1BD-D821B644E67D}"
let Bob = PlayerName "Bob"
let Fred = PlayerName "Fred"
let SBD = DeckName "Soo Bahk Do"
let players =
    [
        { PlayerConfig.Name = Bob; Deck = SBD }
        { PlayerConfig.Name = Fred; Deck = SBD }
    ]

type State = {
        Generation : int
        Game : Game
    }

let eventFold result event =
    result |> bind (fun state ->
        Game.Events.processEvent event state.Game
        |> lift (fun g -> { Generation = state.Generation + 1; Game = g }))

let foldEvents state events =
    state
    |> bind (fun s ->
                ok <| write (s.Generation) events) 
    |> ignore
    List.fold eventFold state events

let commandFold state command =
    state |> bind (fun s ->
        s.Game
        |> Game.Commands.processCommand command
        |> bind (fun events ->
            foldEvents state events))

let foldCommands state =
    List.fold commandFold state

let events =
    [
        Commands.StartGame (gid, players)
        Commands.PlayInitiative (Bob, Basic(Punch, 1))
        Commands.PlayInitiative (Fred, Basic(Kick, 3))
        Commands.PlayAttack (Bob, Fred, [Basic (Punch, 3)])
        Commands.Counter (Fred, [])
        Commands.MoveToStance (Bob, Some <| Basic (Defend, 4))
        Commands.PlayAttack (Fred, Bob, [])
        Commands.MoveToStance (Fred, None)
        Commands.PlayAttack (Bob, Fred, [Basic(Kick, 5)])
        Commands.Counter(Fred, [Basic(Defend, 4)])
        Commands.MoveToStance (Bob, Some (Basic(Defend,3)))
        Commands.PlayAttack (Fred, Bob, [Basic(Punch, 2)])
        Commands.Counter (Bob, [])
        Commands.MoveToStance (Fred, Some <| KnockDown)
        Commands.PlayAttack(Bob, Fred, [Basic(Throw, 5)])
        Commands.Counter(Fred, [Basic(Punch, 1)])
        Commands.Counter(Bob, [])
        Commands.MoveToStance(Bob, Some <| MegaAttack("Earth Sink", Defend, Punch))
        Commands.PlayAttack(Fred, Bob, [Basic(Throw, 6);KnockDown])
        Commands.Counter(Bob, [])
    ]

let start = ok { Generation = -1; Game = Nothing }

//let state1 = foldCommands start events

let foldEvents2 state events =
    Seq.fold eventFold state events

let rec readEvents i : GameEvent seq =
    seq {
        let slice = store.ReadStreamEventsForwardAsync("game-26bc09f9-4585-45b3-a1bd-d821b644e67d", i, 10, true).Result
        let events =
            slice.Events
            |> Seq.map (fun e -> e.Event.Data)
        events |> Seq.iteri (fun i e -> printfn "%d - %A" i e)
        yield!
            events
            |> Seq.map decode
        if not slice.IsEndOfStream then
            yield! readEvents (slice.NextEventNumber)
    }

let reloaded =
    readEvents 0
    |> foldEvents2 start

//store.Dispose()