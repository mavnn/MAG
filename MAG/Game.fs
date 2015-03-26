[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MAG.Game

open System
open MAG
open MAG.Decks
open Chessie.ErrorHandling

let gameId gameState =
    match gameState with
    | InProgress ip -> ok ip.Id
    | Start init -> ok init.Id
    | Complete won -> ok won.Id
    | Nothing -> fail ``Game does not exist``

let getSeed gameState =
    match gameState with
    | InProgress ip -> ok ip.Seed
    | Start init -> ok init.Seed
    | Complete won -> ok won.Seed
    | Nothing -> fail ``Game does not exist``

let swapSeed seed gameState =
    match gameState with
    | InProgress ip ->
        InProgress { ip with Seed = seed } |> ok
    | Start init ->
        Start { init with Seed = seed } |> ok
    | Complete won ->
        Complete { won with Seed = seed } |> ok
    | Nothing -> fail ``Game does not exist``

let getPlayers gameState =
    match gameState with
    | InProgress { Players = ps }
    | Start { Players = ps }
    | Complete { Players = ps } ->
        ok ps
    | Nothing -> fail ``Game does not exist``

let getPlayer playerName gameState =
    trial {
        let! players = getPlayers gameState
    
        match players |> Map.tryFind playerName with
        | Some p -> return p
        | None -> return! fail ``Playing not taking part in this game``
    }

let swapPlayer player gameState =
    match gameState with
    | InProgress ip ->
        InProgress { ip with Players = Map.add player.Name player ip.Players }
        |> ok
    | Start init ->
        Start { init with Players = Map.add player.Name player init.Players }
        |> ok
    | Complete won ->
        Complete { won with Players = Map.add player.Name player won.Players }
        |> ok
    | Nothing -> fail ``Game does not exist``

let shuffle playerName gameState =
    let mix cards seed =
        let rand = Random(seed)
        let arr = cards |> List.toArray
        for i in 0 .. (arr.Length - 1) do
            let j = rand.Next(i, arr.Length)
            let v = arr.[j]
            arr.[j] <- arr.[i]
            arr.[i] <- v
        arr |> Array.toList
    trial {
        let! p = getPlayer playerName gameState
        let! seed = getSeed gameState
        let! g = swapPlayer { p with Deck = mix p.Deck seed } gameState

        return! swapSeed (seed + 1) g
    }

let draw i playerName game =
    let rec inner cards i playerName game =
        trial {
            match i with
            | 0uy -> return cards
            | x ->
                let! player = getPlayer playerName game
                match player with
                | { Deck = [] } ->
                    return!
                        game
                        |> swapPlayer { player with Deck = player.Discards; Discards = [] }
                        >>= shuffle playerName
                        >>= inner cards i playerName 
                | { Deck = h::t } ->
                    return!
                        swapPlayer { player with Deck = t; Hand = h::player.Hand } game
                        >>= inner (h::cards) (i - 1uy) playerName 
        }
    inner [] i playerName game

let initiativeAllPlayed init player card =
    ((player, card)::init.InitiativeCards |> List.map fst |> List.sort) = 
        (init.Players |> Map.toList |> List.map fst |> List.sort)

let orderInitiative cards =
    cards
    |> List.map (fun (pn, card) ->
        match card with
        | Basic(_, v) ->
            ok (pn, v)
        | KnockDown
        | Combo(_, _)
        | MegaAttack(_, _, _) -> fail ``Invalid initiative card``)
    |> collect
    |> (lift (Seq.sortBy (fun (_, v) -> v) >> List.ofSeq))

let nextPlayer gameState =
    match gameState with
    | Complete _
    | Start _ ->
        fail ``Game is not in progress``
    | InProgress openGame ->
        let rec inner turn players =
            match players with
            | current::next::t when current = turn ->
                trial {
                    let! p = getPlayer next gameState
                    match p with
                    | { Life = life } when life <= 0 ->
                        return! inner next (next::t)
                    | _ -> return next
                }
            | [current] -> ok (List.head openGame.TurnOrder)
            | [] -> fail ``Unexpected error``
            | h::t -> inner turn t
        inner openGame.Turn openGame.TurnOrder
    | Nothing ->
        fail ``Game does not exist``

let existsWinner gameState =
    trial {
        let! players = getPlayers gameState
        let live =
            players
            |> Map.toList
            |> List.map snd
            |> List.filter (fun p -> p.Life > 0)
        return
            if List.length live = 1 then
                Some (List.head live)
            else None
    }

let create gid seed playerConfigs =
    let players =
        playerConfigs
        |> List.map (fun (config : PlayerConfig) ->
            config.Name, {
                Name = config.Name
                Life = 20
                Deck = Decks.[config.Deck]
                Hand = []
                Stance = []
                Discards = []
            })
        |> Map.ofList
    let start =
        Start {
            Id = gid
            Seed = seed
            Players = players
            InitiativeCards = []
        } |> ok
    playerConfigs
    |> List.fold (fun g pc -> g |> bind (fun g -> shuffle pc.Name g)) start