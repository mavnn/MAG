// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load @"load-project.fsx"
open MAG
open MAG.Domain
open MAG.Decks
open Chessie.ErrorHandling

let Bob = PlayerName "Bob"
let Fred = PlayerName "Fred"

let one = 
    Game.create [
            Bob, DeckName "Soo Bank Do"
            Fred, DeckName "Jiu Jitsu"
        ] 1

//let bobNearlyWon =
//    one
//    >>= Game.playInitiative Bob (BasicPunch 1uy)
//    >>= Game.playInitiative Fred (BasicPunch 3uy)
//    >>= Game.playAction Bob Fred [BasicPunch 2uy]
//    >>= Game.playAction Fred Bob [BasicDefend 1uy]
//    >>= Game.playStance Bob [BasicThrow 6uy]
//    >>= Game.playAction Fred Bob [BasicPunch 4uy]
//    >>= Game.playAction Bob Fred [BasicKick 4uy]
//    >>= Game.playAction Fred Bob [BasicThrow 2uy]
//    >>= Game.playAction Bob Fred [BasicDefend 2uy]
//    >>= Game.playStance Fred [BasicDefend 5uy]
//    >>= Game.playAction Bob Fred [KnockDown;BasicThrow 6uy]
//    >>= Game.playAction Fred Bob []
//    >>= Game.playStance Bob [BasicDefend 5uy]
//    >>= Game.playAction Fred Bob [BasicKick 5uy]
//    >>= Game.playAction Bob Fred [BasicDefend 5uy]
//    >>= Game.playStance Fred []
//    >>= Game.playAction Bob Fred [BasicKick 3uy]
//    >>= Game.playAction Fred Bob []
//    >>= Game.playStance Bob [Combo (Punch, Kick)]
//    >>= Game.playAction Fred Bob []
//    >>= Game.playStance Fred [BasicThrow 3uy]
//    >>= Game.playAction Bob Fred [BasicThrow 5uy]
//    >>= Game.playAction Fred Bob []
//    >>= Game.playStance Bob [BasicPunch 4uy]
//    >>= Game.playAction Fred Bob []
//    >>= Game.playStance Fred []
//    >>= Game.playAction Bob Fred [BasicKick 5uy;BasicPunch 4uy;Combo(Punch, Kick)]
//
//bobNearlyWon
//>>= Game.playAction Fred Bob []

let McBob = PlayerName "McBob"

let two =
    Game.create [
        McBob, SooBahkDo
        Bob, SooBahkDo
        Fred, JiuJitsu    
    ] 2

two
>>= Game.playInitiative McBob (Basic(Kick, 4uy))
>>= Game.playInitiative Bob (Basic(Kick, 2uy))
>>= Game.playInitiative Fred (Basic(Defend, 3uy))