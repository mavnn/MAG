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

type CardMoved =
    {
        Player : PlayerName
        Card : Card
    }

type DamageTaken =
    {
        Player : PlayerName
        Damage : int        
    }

type Command = Command of string

type GameEvent =
    | Created of GameCreated
    | Played of CardMoved
    | Drawn of CardMoved
    | DamageTaken of DamageTaken
    | KnockedDown of PlayerName
    | Target of Target : PlayerName
    | PhaseComplete
    | CommandRecieved of Command

// Serialization
open Aether
open Aether.Operators
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
            fun x ->
                match x with
                | "punch" -> Json.init Punch
                | "kick" -> Json.init Kick
                | "throw" -> Json.init Throw
                | "defend" -> Json.init Defend
                | s -> Json.error (sprintf "Expected suit, found %A" s)
        =<< Json.getLensPartial (idLens <-?> Json.StringPIso)

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
            fun x -> PlayerName x
        <!> Json.getLensPartial (idLens <-?> Json.StringPIso)

type DeckName with
    static member ToJson (DeckName d) =
        ToJsonDefaults.ToJson d
    static member FromJson (_ : DeckName) =
            fun x -> DeckName x
        <!> Json.getLensPartial (idLens <-?> Json.StringPIso)

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

type CardMoved with
    static member ToJson (t : CardMoved) =
        Json.write "player" t.Player
        *> Json.write "card" t.Card
    static member FromJson (_ : CardMoved) =
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

type Command with
    static member ToJson (Command c) =
        ToJsonDefaults.ToJson c
    static member FromJson (_ : Command) =
            fun s -> Command s
        <!> Json.getLensPartial (idLens <-?> Json.StringPIso)

type GameEvent with
    static member ToJson (g : GameEvent) =
        match g with
        | Created c ->
            Json.write "eventType" "GameCreated"
            *> Json.write "eventData" c
        | Played p ->
            Json.write "eventType" "CardPlayed"
            *> Json.write "eventData" p
        | Drawn d -> 
            Json.write "eventType" "CardDrawn"
            *> Json.write "eventData" d
        | DamageTaken d ->
            Json.write "eventType" "DamageTaken"
            *> Json.write "eventData" d
        | KnockedDown p ->
            Json.write "eventType" "KnockedDown"
            *> Json.write "eventData" p
        | Target t -> 
            Json.write "eventType" "Target"
            *> Json.write "eventData" t
        | PhaseComplete ->
            Json.write "eventType" "PhaseComplete"
        | CommandRecieved c ->
            Json.write "eventType" "CommandRecieved"
            *> Json.write "eventData" c
    static member FromJson (_ : GameEvent) =
        json {
            let! t = Json.read "eventType"
            match t with
            | "GameCreated" ->
                let! d = Json.read "eventData"
                return Created d
            | "CardPlayed" ->
                let! d = Json.read "eventData"
                return Played d
            | "CardDrawn" ->
                let! d = Json.read "eventData"
                return Drawn d
            | "DamageTaken" ->
                let! d = Json.read "eventData"
                return DamageTaken d
            | "KnockedDown" ->
                let! d = Json.read "eventData"
                return KnockedDown d
            | "Target" ->
                let! d = Json.read "eventData"
                return Target d
            | "PhaseComplete" ->
                return PhaseComplete
            | "CommandRecieved" ->
                let! d = Json.read "eventData"
                return CommandRecieved d
            | c ->
                return! Json.error (sprintf "Expected game event type, found %A" c)
        }
