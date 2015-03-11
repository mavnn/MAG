[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MAG.Player

open MAG
open Chessie.ErrorHandling

let private from held cards =
    held |> List.partition (fun c -> List.exists ((=) c) cards)

let play cards player =
    match from player.Hand cards, from player.Stance cards with
    | _ when cards = [] ->
        ok player
    | ([], handRemaining), ([], stanceRemaining) ->
        fail (``Playing cards you don't have`` player)
    | (_::_::t, _), (_, _) ->
        fail (``Cannot play more than one card from your hand in a single action`` player)
    | (handPlayed, handRemaining), (stancePlayed, stanceRemaining) ->
        let available = List.concat [handPlayed;stancePlayed]
        if cards |> List.forall (fun card -> available |> List.exists ((=) card)) then
            ok { player with
                            Hand = handRemaining
                            Stance = stanceRemaining
                            Discards = List.concat [cards;player.Discards] }
        else
            fail (``Playing cards you don't have`` player)

let putStance card player =
    match from player.Hand [card] with
    | ([], _) ->
        fail (``Playing cards you don't have`` player)
    | (_, handRemaining) ->
        ok { player with Hand = handRemaining; Stance = card::player.Stance }            

