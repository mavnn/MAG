#r @"System.dll"
#r @"System.Core.dll"
#r @"System.Numerics.dll"
#r @"..\..\packages\Aether\lib\net40\Aether.dll"
#r @"..\..\packages\Chiron\lib\net40\Chiron.dll"
#r @"..\..\packages\FParsec\lib\net40-client\FParsec.dll"
#r @"..\..\packages\FParsec\lib\net40-client\FParsecCS.dll"
#r @"..\..\packages\Chessie\lib\net40\Chessie.dll"
#r @"..\..\MAG.Events\bin\Debug\MAG.Events.dll"
#load @"..\Domain.fs"
      @"..\Card.fs"
      @"..\Player.fs"
      @"..\Action.fs"
      @"..\Decks.fs"
      @"..\Game.fs"
      @"..\Commands.fs"
      @"..\GameEvents.fs"
      @"..\GameCommands.fs"

open MAG
open Chiron
open Chessie.ErrorHandling

let gid = GameId <| System.Guid.Parse "{c0455a38-c87e-4570-b635-fb1f220c4164}"
let Bob = PlayerName "Bob"
let Fred = PlayerName "Fred"
let SBD = DeckName "Soo Bahk Do"
let JJ = DeckName "Jiu Jitsu"
let players =
    [
        { PlayerConfig.Name = Bob; Deck = SBD }
        { PlayerConfig.Name = Fred; Deck = SBD }
    ]

let eventFold sideEffect result event =
    result
    |> bind (fun g ->
        sideEffect event
        GameEvents.processEvent g event)

let commandFold sideEffect result command =
    let events =
        result
        |> bind (fun g -> GameCommands.processCommand g command)
    events
    |> bind (fun es -> es
                       |> List.fold (eventFold sideEffect) result)

let foldCommands sideEffect state =
    List.fold (commandFold sideEffect) state

let start = ok Nothing

let commands =
    [
        Commands.StartGame (gid, players)
        Commands.PlayInitiative (Bob, Basic(Punch, 1))
        Commands.PlayInitiative (Fred, Basic(Punch, 5))
    ]

let current = foldCommands (printfn "%A") start commands
