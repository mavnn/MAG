[<AutoOpen>]
module MAG.Events

open System

type Suit =
    | Punch
    | Kick
    | Throw
    | Defend

type Card =
    | KnockDown
    | Combo of Speed : Suit * Extra : Suit
    | MegaAttack of Name : string * Speed : Suit * Damage : Suit
    | Basic of Suit * int

type PlayerName = PlayerName of string

type DeckName = DeckName of string

type PlayerConfig =
    {
        Name : PlayerName
        Deck : DeckName
    }

type GameCreated =
    {
        Id : Guid
        Seed : int
        Players : PlayerConfig list
    }

type CardPlayed =
    {
        Player : PlayerName
        Card : Card
    }

type CardDrawn =
    {
        Player : PlayerName
        Card : Card
    }

type DamageTaken =
    {
        Player : PlayerName
        Damage : int        
    }

type GameEvent =
    | Created of GameCreated
    | Played of CardPlayed
    | Drawn of CardDrawn
    | DamageTaken of DamageTaken
    | Target of Target : PlayerName
    | PhaseComplete

// Serialization
open Chiron
open Chiron.Operators

type Suit with
    static member ToJson (suit : Suit) =
        match suit with
        | Punch -> ToJsonDefaults.ToJson "punch"
        | Kick -> ToJsonDefaults.ToJson "kick"
        | Throw -> ToJsonDefaults.ToJson "throw"
        | Defend -> ToJsonDefaults.ToJson "defend"
    static member FromJson (_ : Suit) : Json<Suit> =
        fun json ->
            match json with
            | String "punch" -> Value Punch, json
            | String "kick" -> Value Kick, json
            | String "throw" -> Value Throw, json
            | String "defend" -> Value Defend, json
            | s -> Error (sprintf "Expected suit, found %A" s), json

type Card with
    static member private createMega name speed damage =
        MegaAttack (name, speed, damage)

    static member private createBasic suit value =
        Basic (suit, value)

    static member ToJson (card : Card) =
        match card with
        | KnockDown ->
            Json.write "type" "knockdown"
        | Combo (speed, extra) -> 
            Json.write "type" "combo"
            *> Json.write "speed" speed
            *> Json.write "extra" extra
        | MegaAttack (name, speed, damage) ->
            Json.write "type" "mega"
            *> Json.write "name" name
            *> Json.write "speed" speed
            *> Json.write "damage" damage
        | Basic (suit, v) ->
            Json.write "type" "basic"
            *> Json.write "suit" suit
            *> Json.write "value" v
    static member FromJson (_ : Card) =
        json {
            let! t = Json.read "type"

            return!
                match t with
                | "knockdown" -> Json.init KnockDown
                | "combo" ->
                        fun spd ext -> Combo(spd, ext)
                    <!> (Json.read "speed") <*> (Json.read "extra")
                | "mega" ->
                        fun name spd dam -> MegaAttack (name, spd, dam)
                    <!> (Json.read "name") <*> (Json.read "speed") <*> (Json.read "damage")
                | "basic" -> Card.createBasic <!> (Json.read "suit") <*> (Json.read "value")
                | _ -> Json.error (sprintf "Expected card")
        }

type PlayerName with
    static member ToJson (PlayerName p) =
        ToJsonDefaults.ToJson p
    static member FromJson (_ : PlayerName) : Json<PlayerName> =
        fun json ->
            match json with
            | String s -> Value <| PlayerName s, json
            | x -> Error (sprintf "Expected player name, found %A" x), json

type DeckName with
    static member ToJson (DeckName d) =
        ToJsonDefaults.ToJson d
    static member FromJson (_ : DeckName) =
        fun json ->
            match json with
            | String s -> Value <| DeckName s, json
            | x -> Error (sprintf "Expected deck name, found %A" x), json

type PlayerConfig with
    static member ToJson (c : PlayerConfig) =
        Json.write "name" c.Name
        *> Json.write "deck" c.Deck
    static member FromJson (_ : PlayerConfig) =
            fun name deck -> { Name = name; Deck = deck }
        <!> Json.read "name"
        <*> Json.read "deck"
            
type GameCreated with
    static member ToJson (g : GameCreated) =
        Json.write "id" (sprintf "%A" g.Id)
        *> Json.write "players" g.Players
        *> Json.write "seed" g.Seed

    static member FromJson (_ : GameCreated) =
            fun id players seed -> { Id = id; Players = players; Seed = seed }
        <!> Json.read "id"
        <*> Json.read "players"
        <*> Json.read "seed"

type CardPlayed with
    static member ToJson (t : CardPlayed) =
        Json.write "player" t.Player
        *> Json.write "card" t.Card
    static member FromJson (_ : CardPlayed) =
            fun player card -> { Player = player; Card = card }
        <!> Json.read "player"
        <*> Json.read "card"

type CardDrawn with
    static member ToJson (t : CardDrawn) =
        Json.write "player" t.Player
        *> Json.write "card" t.Card
    static member FromJson (_ : CardDrawn) =
            fun player card -> { Player = player; Card = card }
        <!> Json.read "player"
        <*> Json.read "card"

type DamageTaken with
    static member ToJson (t : DamageTaken) =
        Json.write "player" t.Player
        *> Json.write "damage" t.Damage
    static member FromJson (_ : DamageTaken) =
            fun player damage -> { Player = player; Damage = damage }
        <!> Json.read "player"
        <*> Json.read "damage"

type GameEvent with
    static member ToJson (g : GameEvent) =
        match g with
        | Created c ->
            Json.write "eventType" "GameCreated"
            *> Json.write "eventData" c
        | Played(_) -> failwith "Not implemented yet"
        | Drawn(_) -> failwith "Not implemented yet"
        | DamageTaken(_) -> failwith "Not implemented yet"
        | Target(target) -> failwith "Not implemented yet"
        | PhaseComplete -> failwith "Not implemented yet"
