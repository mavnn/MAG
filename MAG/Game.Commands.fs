module MAG.Game.Commands

open Chessie.ErrorHandling
open MAG
open MAG.Commands
open MAG.Events
open MAG.Game

let processStartGame (guid : System.Guid) players =
    trial {
        let allDecks =
            players
            |> List.map (fun c -> c.Deck)
            |> List.map (fun d -> Decks.Decks.TryFind d)
            |> List.map (fun o -> match o with Some o -> true | None -> false)
            |> List.forall id
        if allDecks then
            let seed =
                guid.ToByteArray()
                |> Array.map int
                |> Array.reduce (+)
            let created = Created { Id = guid; Seed = seed; Players = players }
            let! startState = create guid seed players
            let! draws =
                [for p in players |> List.map (fun c -> c.Name) ->
                    draw 4uy p startState
                    |> lift (fun c -> [for card in c -> (p, card)]) ]
                |> collect
                |> lift (List.concat)
                |> lift (List.map (fun (p, c) -> Drawn { Player = p; Card = c }))
            return created::draws
        else
            return! fail ``Deck does not exist``
    }

let processPlayInitiative player card game =
    match game with
    | Start init ->
        trial {
            let! playedCard =
                if init.InitiativeCards |> List.map fst |> List.exists ((=) player) then
                    fail ``You can only play one initiative card``
                else if (match card with Basic _ -> false | _ -> true) then
                    fail ``Invalid initiative card``
                else
                    getPlayer player game
                    >>= Player.play [card]
                    >>= (fun _ -> ok <| Played { Player = player; Card = card})

            if initiativeAllPlayed init player card then
                let! hasDraws =
                    (player, card)::init.InitiativeCards
                    |> orderInitiative
                    |> lift (List.map snd)
                    |> lift (fun ints -> Seq.distinct ints |> Seq.toList <> ints)
                if hasDraws then
                    let! draws =
                        [for p in init.Players |> Map.toList |> List.map fst ->
                            draw 1uy p game
                            |> lift (fun c -> p, List.head c)]
                        |> collect
                        |> lift (List.map (fun (p, c) -> Drawn { Player = p; Card = c }))
                    return playedCard::draws
                else
                    return [playedCard;PhaseComplete]
            else
                return [playedCard]
        }
    | Nothing ->
        fail ``Game does not exist``
    | _ ->
        fail ``Initiative phase has finished``

let processPlayAttack player target cards game =
    match game with
    | InProgress openGame ->
        match openGame.Phase with
        | Play (Some _, _) ->
            fail ``Playing out of turn``
        | Play (None, []) ->
            trial {
                if openGame.Turn <> player then
                    return! fail ``Playing out of turn``
                else
                    match cards with
                    | [] ->
                        return [PhaseComplete]
                    | _ ->
                        let played =
                            cards
                            |> List.map (fun c -> Played { Player = player; Card = c})
                        let! drawn =
                            getPlayer player game
                            >>= Player.play cards
                            |> lift (fun (fromHand, _) ->
                                        fromHand |> List.length |> byte)
                            >>= (fun i -> draw i player game)
                            |> lift (List.map (fun c -> Drawn { Player = player; Card = c }))

                        return 
                            [
                                [Target target]
                                played
                                drawn
                                [PhaseComplete]
                            ] |> List.concat
            }

        | _ ->
            fail ``Playing out of turn``
    | _ ->
        fail ``Playing out of turn``

let processCounter player cards game =
    match game with
    | InProgress openGame ->
        match openGame.Phase with
        | Respond (target, attack, []) ->
            trial {
                if target <> player then
                    return! fail ``Playing out of turn``
                else if cards = [] then
                    let kd =
                        match attack.Effect with
                        | Some KnockDownEffect ->
                            [KnockedDown target]
                        | None -> []
                    let damage =
                        [DamageTaken { Player = target; Damage = attack.Damage / 1<damage> }]
                    return
                        [kd;damage;[PhaseComplete]] |> List.concat
                else
                    let played =
                        cards
                        |> List.map (fun c -> Played { Player = player; Card = c})
                    let! counter =
                        Card.toAction player cards
                    if Action.isCounter attack counter then
                        let! drawn =
                            getPlayer player game
                            >>= Player.play cards
                            |> lift (fun (fromHand, _) ->
                                        fromHand |> List.length |> byte)
                            >>= (fun i -> draw i player game)
                            |> lift (List.map (fun c -> Drawn { Player = player; Card = c }))

                        return 
                            [
                                played
                                drawn
                                [PhaseComplete]
                            ] |> List.concat
                    else
                        return! fail ``Response is not a counter``
            }

        | _ ->
            fail ``Playing out of turn``
    | _ ->
        fail ``Playing out of turn``

let processMoveToStance player card game =
    match game with
    | InProgress openGame ->
        match openGame.Phase with
        | Stance ->
            if player <> openGame.Turn then
                fail ``Playing out of turn``
            else
                trial {
                    match card with
                    | None ->
                        return [PhaseComplete]
                    | Some c ->
                        let! p = getPlayer player game
                        if List.exists ((=) c) p.Hand then
                            let played = Played { Player = player; Card = c }
                            let! drawn =
                                draw 1uy player game
                                |> lift (List.head)
                                |> lift (fun c' -> Drawn { Player = player; Card = c' })
                            return [played;drawn;PhaseComplete]
                        else
                            return! fail ``Playing cards you don't have`` 

                }
        | _ ->
            fail ``Playing out of turn``
    | _ ->
        fail ``Playing out of turn``

let processCommand command game =
    match command with
    | StartGame (guid, players) ->
        processStartGame guid players
    | PlayInitiative(player, card) ->
        processPlayInitiative player card game
    | PlayAttack(player, target, cards) ->
        processPlayAttack player target cards game
    | Counter(player, cards) ->
        processCounter player cards game
    | MoveToStance(player, card) ->
        processMoveToStance player card game

