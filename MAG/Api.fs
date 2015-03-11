module MAG.Api

open MAG

type Command =
    | PlayInitiative of PlayerName * Card
    | PlayAction of Player : PlayerName * Target : PlayerName * Card list
    | PutInStance of PlayerName * Card option

let fold state command =
    match command with
    | PlayInitiative (player, card) ->
        Game.playInitiative player card state
    | PlayAction (player, target, cards) ->
        Game.playAction player target cards state
    | PutInStance (player, card) ->
        match card with
        | Some c ->
            Game.playStance player [c] state
        | None ->
            Game.playStance player [] state
