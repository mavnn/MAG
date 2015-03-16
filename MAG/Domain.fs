[<AutoOpen>]
module MAG.Domain

open System
open MAG.Events

type Player =
    {
        Name : PlayerName
        Life : int
        Deck : Card list
        Hand : Card list
        Stance : Card list
        Discards : Card list
    }

[<Measure>]
type speed

[<Measure>]
type damage

type ExtraEffect =
    | KnockDownEffect

type Action =
    {
        Suit : Suit
        Speed : int<speed>
        Damage : int<damage>
        Effect : ExtraEffect option
        Originator : PlayerName
    }

type Phase =
    | Play of Target : PlayerName option * Card list
    | Respond of PlayerName * Action * Card list
    | Stance

type Initiative =
    {
        Id : Guid
        Players : Map<PlayerName, Player>
        InitiativeCards : (PlayerName * Card) list
        Seed : int
    }

type OpenGame =
    {
        Id : Guid
        Turn : PlayerName
        Players : Map<PlayerName, Player>
        TurnOrder : PlayerName list
        Phase : Phase
        Seed : int
    }

type Winner =
    {
        Id : Guid
        Winner : PlayerName
        Players : Map<PlayerName, Player>
        Seed : int
    }

type Game =
    | Nothing
    | Start of Initiative
    | InProgress of OpenGame
    | Complete of Winner

type Errors =
    | ``Game does not exist``
    | ``Game already exists``
    | ``Deck does not exist``
    | ``Playing cards you don't have``
    | ``Cannot play more than one card from your hand in a single action``
    | ``Knockdown can only be used with attack actions``
    | ``Wrong suits``
    | ``Cards do not form an action``
    | ``Initiative phase has finished``
    | ``You can only play one initiative card``
    | ``Invalid initiative card``
    | ``Playing not taking part in this game``
    | ``Playing out of turn``
    | ``Game is not in progress``
    | ``You can only put one card in your stance``
    | ``Response is not a counter``
    | ``Unexpected error``