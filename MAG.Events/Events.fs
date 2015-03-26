[<AutoOpen>]
module MAG.Events

type GameCreated =
    {
        Id : GameId
        Seed : int
        Players : PlayerConfig list
    }

type CardMoved =
    {
        Id : GameId
        Player : PlayerName
        Card : Card
    }

type DamageTaken =
    {
        Id : GameId
        Player : PlayerName
        Damage : int        
    }

type Command =
    {
        Id : GameId
        Command : string
    }

type KnockedDown =
    {
        Id : GameId
        Target : PlayerName
    }

type Target =
    {
        Id : GameId
        Target : PlayerName
    }

type PhaseComplete =
    {
        Id : GameId
    }

type GameEvent =
    | Created of GameCreated
    | Played of CardMoved
    | Drawn of CardMoved
    | DamageTaken of DamageTaken
    | KnockedDown of KnockedDown
    | Target of Target
    | PhaseComplete of PhaseComplete
    | CommandRecieved of Command
    member x.Id =
        match x with
        | Created c -> c.Id
        | Played p -> p.Id
        | Drawn d -> d.Id
        | DamageTaken dt -> dt.Id
        | KnockedDown kd -> kd.Id
        | Target t -> t.Id
        | PhaseComplete p -> p.Id
        | CommandRecieved cr -> cr.Id

// Serialization
open Aether
open Aether.Operators
open Chiron
open Chiron.Operators

            
type GameCreated with
    static member ToJson (g : GameCreated) =
        Json.write "id" g.Id
        *> Json.write "players" g.Players
        *> Json.write "seed" g.Seed

    static member FromJson (_ : GameCreated) =
            fun id players seed -> { Id = id; Players = players; Seed = seed }
        <!> Json.read "id"
        <*> Json.read "players"
        <*> Json.read "seed"

type CardMoved with
    static member ToJson (t : CardMoved) =
        Json.write "id" t.Id
        *> Json.write "player" t.Player
        *> Json.write "card" t.Card
    static member FromJson (_ : CardMoved) =
            fun gid player card -> { Id = gid; Player = player; Card = card }
        <!> Json.read "id"
        <*> Json.read "player"
        <*> Json.read "card"

type DamageTaken with
    static member ToJson (t : DamageTaken) =
        Json.write "id" t.Id
        *> Json.write "player" t.Player
        *> Json.write "damage" t.Damage
    static member FromJson (_ : DamageTaken) =
            fun gid player damage -> { Id = gid; Player = player; Damage = damage }
        <!> Json.read "id"
        <*> Json.read "player"
        <*> Json.read "damage"

type Command with
    static member ToJson (c : Command) =
        Json.write "id" c.Id
        *> Json.write "command" c.Command        
    static member FromJson (_ : Command) =
            fun gid c -> { Id = gid; Command = c }
        <!> Json.read "id"
        <*> Json.read "command"

type KnockedDown with
    static member ToJson (k : KnockedDown) =
        Json.write "id" k.Id
        *> Json.write "target" k.Target
    static member FromJson (_ : KnockedDown) =
            fun gid t -> { Id = gid; Target = t }
        <!> Json.read "id"
        <*> Json.read "target"

type Target with
    static member ToJson (t : Target) =
        Json.write "id" t.Id
        *> Json.write "target" t.Target
    static member FromJson (_ : Target) =
            fun gid t -> { Id = gid; Target = t }
        <!> Json.read "id"
        <*> Json.read "target"

type PhaseComplete with
    static member ToJson (p : PhaseComplete) =
        Json.write "id" p.Id
    static member FromJson (_ : PhaseComplete) =
            fun gid -> { Id = gid }
        <!> Json.read "id"

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
        | PhaseComplete p ->
            Json.write "eventType" "PhaseComplete"
            *> Json.write "eventData" p
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
                let! p = Json.read "eventData"
                return PhaseComplete p
            | "CommandRecieved" ->
                let! d = Json.read "eventData"
                return CommandRecieved d
            | c ->
                return! Json.error (sprintf "Expected game event type, found %A" c)
        }
