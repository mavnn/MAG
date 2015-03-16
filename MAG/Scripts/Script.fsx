// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

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
      @"..\Game.Events.fs"
      @"..\Game.Commands.fs"
open MAG
open Chessie.ErrorHandling

let gid = System.Guid.Parse "{26BC09F9-4585-45B3-A1BD-D821B644E67D}"
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

let foldEvents state =
    List.fold eventFold state

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

let start = ok { Generation = 0; Game = Nothing }

let state1 = foldCommands start events
