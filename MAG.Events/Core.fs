[<AutoOpen>]
module MAG.Core

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

type GameId = GameId of Guid

type PlayerName = PlayerName of string

type DeckName = DeckName of string

type PlayerConfig =
    {
        Name : PlayerName
        Deck : DeckName
    }

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

type GameId with
    static member ToJson (GameId gid) =
        ToJsonDefaults.ToJson gid
    static member FromJson (_ : GameId) =
        GameId <!> FromJsonDefaults.FromJson (Guid.NewGuid())

type PlayerConfig with
    static member ToJson (c : PlayerConfig) =
        Json.write "name" c.Name
        *> Json.write "deck" c.Deck
    static member FromJson (_ : PlayerConfig) =
            fun name deck -> { Name = name; Deck = deck }
        <!> Json.read "name"
        <*> Json.read "deck"
