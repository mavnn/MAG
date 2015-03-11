[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MAG.Game

open System
open MAG
open MAG.Decks
open Chessie.ErrorHandling

let gameId gameState =
    match gameState with
    | InProgress ip -> ip.Id
    | Start init -> init.Id
    | Complete won -> won.Id

let getSeed gameState =
    match gameState with
    | InProgress ip -> ip.Seed
    | Start init -> init.Seed
    | Complete won -> won.Seed

let swapSeed seed gameState =
    match gameState with
    | InProgress ip ->
        InProgress { ip with Seed = seed }
    | Start init ->
        Start { init with Seed = seed }
    | Complete won ->
        Complete { won with Seed = seed }

let getPlayer playerName gameState =
    match gameState with
    | InProgress { Players = ps }
    | Start { Players = ps }
    | Complete { Players = ps } ->
        match ps |> Map.tryFind playerName with
        | Some p -> ok p
        | None -> fail <| ``Playing not taking part in this game`` playerName

let swapPlayer player gameState =
    match gameState with
    | InProgress ip ->
        InProgress { ip with Players = Map.add player.Name player ip.Players }
    | Start init ->
        Start { init with Players = Map.add player.Name player init.Players }
    | Complete won ->
        Complete { won with Players = Map.add player.Name player won.Players }

let shuffle playerName gameState =
    let mix seed cards =
        let rand = Random(seed)
        let arr = cards |> List.toArray
        for i in 0 .. (arr.Length - 1) do
            let j = rand.Next(i, arr.Length)
            let v = arr.[j]
            arr.[j] <- arr.[i]
            arr.[i] <- v
        arr |> Array.toList
    getPlayer playerName gameState
    |> lift (fun p -> swapPlayer { p with Deck = mix (getSeed gameState) p.Deck } gameState)
    |> lift (swapSeed (getSeed gameState + 1))

let rec draw i playerName game =
    trial {
        match i with
        | 0uy -> return game
        | x ->
            let! player = getPlayer playerName game
            match player with
            | { Deck = [] } ->
                return!
                    game
                    |> swapPlayer { player with Deck = player.Discards; Discards = [] }
                    |> shuffle playerName
                    >>= draw i playerName 
            | { Deck = h::t } ->
                return!
                    swapPlayer { player with Deck = t; Hand = h::player.Hand } game
                    |> draw (i - 1uy) playerName 
    }

let create (setup : (PlayerName * DeckName) list) start =
    let initialState =
        setup
        |> List.map (fun (name, deck) ->
            {
                Name = name
                Life = 20
                Hand = []
                Deck = Map.find deck Decks
                Stance = []
                Discards = []
            })
        |> List.map (fun p -> p.Name, p)
        |> Map.ofList
        |> fun ps -> { Id = Guid.NewGuid(); InitiativeCards = []; Seed = start; Players = ps }

    let names =
        initialState.Players
        |> Map.toList
        |> List.map fst

    names
    |> List.fold (fun game (name : PlayerName) ->
        game
        >>= (shuffle name)
        >>= (draw 4uy name)) (ok <| Start initialState)

let playInitiative playerName card gameState =
    match gameState with
    | Start init ->
        trial {
            let! p = 
                getPlayer playerName gameState
                >>= Player.play [card]
            let initCards = (playerName, card)::init.InitiativeCards
            let complete =
                (initCards |> List.map fst |> List.sort) = 
                    (init.Players |> Map.toList |> List.map fst |> List.sort)
            if complete then
                let! initOrder =
                    initCards
                    |> List.map (fun (pn, card) ->
                        match card with
                        | Basic(_, v) ->
                            ok (pn, v)
                        | KnockDown
                        | Combo(_, _)
                        | MegaAttack(_, _, _) -> fail <| ``Invalid initiative card`` (pn, card))
                    |> collect
                    |> (lift (Seq.sortBy (fun (_, v) -> v) >> List.ofSeq))
                return!
                    if (initOrder |> List.map snd |> Set.ofList |> Set.count)
                            <> (List.length initOrder) then
                        init.Players
                        |> Map.toList
                        |> List.map fst
                        |> List.fold (fun game name -> game >>= draw 1uy name) 
                            (ok (Start { init with InitiativeCards = [] }))
                    else
                        InProgress {
                                Id = init.Id
                                Players = init.Players
                                Turn = initOrder |> List.head |> fst
                                TurnOrder = initOrder |> List.map fst
                                Seed = init.Seed
                                Phase = Play
                            }
                        |> swapPlayer p
                        |> ok
            else
                return
                    Start { init with InitiativeCards = initCards }
                    |> swapPlayer p
        }
    | _ ->
        fail ``Initiative phase has finished``

let nextPlayer gameState =
    match gameState with
    | Complete _
    | Start _ ->
        fail ``Game is not in progress``
    | InProgress openGame ->
        let rec inner players =
            match players with
            | current::next::_ when current = openGame.Turn ->
                ok next
            | [current] -> ok (List.head openGame.TurnOrder)
            | [] -> fail ``Unexpected error``
            | h::t -> inner t
        inner openGame.TurnOrder

let (|Player|Responding|PutStance|OtherPlayer|GameNotInPlay|) (playerName, gameState) =
    match gameState with
    | InProgress ({ Phase = Stance; Turn = turnName } as og) when turnName = playerName ->
        PutStance og
    | InProgress ({ Phase = Respond (playerName, attack) } as og) ->
        Responding (attack, og)
    | InProgress ({ Phase = Play; Turn = turnName } as og) ->
        Player og
    | Complete _
    | Start _ ->
        GameNotInPlay
    | _ ->
        OtherPlayer

let playStance playerName cards gameState =
    match playerName, gameState with
    | PutStance openGame ->
        match cards with
        | [] ->
            nextPlayer gameState
            >>= fun next -> ok <| InProgress { openGame with Turn = next; Phase = Play }
        | [card] ->
            trial {
                let! p =
                    getPlayer playerName gameState
                    >>= Player.putStance card
                let! next = nextPlayer gameState
                return!
                    InProgress { openGame with Turn = next; Phase = Play}
                    |> swapPlayer p
                    |> draw (3uy - (p.Hand |> List.length |> uint8)) playerName
            }
        | _ ->
            fail <| ``You can only put one card in your stance`` cards
    | _ ->
        fail ``You cannot put cards in your stance outside your stance phase``

let playAction playerName targetName cards gameState =
    match playerName, gameState with
    | OtherPlayer
    | GameNotInPlay ->
        fail <| ``Playing out of turn`` (playerName, cards)
    | PutStance openGame ->
        fail ``You cannot play an attack in your stance phase``
    | Player openGame ->
        trial {
            match cards with
            | [] ->
                return InProgress { openGame with Phase = Stance }
            | _ ->
                let! p =
                    getPlayer playerName gameState
                    >>= Player.play cards
                let! action = Card.toAction playerName cards
                let phase =
                    match action.Damage with
                    | 0<damage> -> Stance
                    | _ -> Respond (targetName, action)
                return!
                    InProgress { openGame with Phase = phase }
                    |> swapPlayer p
                    |> draw (3uy - (p.Hand |> List.length |> uint8)) playerName
        }
    | Responding (attack, openGame) ->
        trial {
            match cards with
            | [] ->
                let! attacker =
                    getPlayer attack.Originator gameState
                let! defender =
                    getPlayer playerName gameState
                let result =
                    Action.apply { Attacker = attacker; Defender = defender } attack
                match result.Defender.Life with
                | i when i <= 0 ->
                    return
                        Complete {
                            Id = openGame.Id
                            Winner = attacker.Name
                            Players = openGame.Players
                            Seed = openGame.Seed }
                        |> swapPlayer result.Defender
                        |> swapPlayer result.Attacker
                | _ ->
                    return
                        InProgress { openGame with Phase = Stance }
                        |> swapPlayer result.Defender
                        |> swapPlayer result.Attacker
            | _ ->
                do! if targetName <> attack.Originator then
                        fail ``Responses can only target the original attacker``
                    else ok ()
                let! counter = Card.toAction playerName cards
                do! if not <| Action.isCounter attack counter then
                        fail <| ``Response is not a counter`` cards
                    else ok ()
                let! p =
                    getPlayer playerName gameState
                    >>= Player.play cards
                let phase =
                    match counter.Damage with
                    | 0<damage> -> Stance
                    | _ -> Respond (targetName, counter)
                return!
                    InProgress { openGame with Phase = phase }
                    |> swapPlayer p
                    |> draw (3uy - (p.Hand |> List.length |> uint8)) playerName
        }
                