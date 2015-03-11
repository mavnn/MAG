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

type Involved =
    {
        Attacker : Player
        Defender : Player
    }

type ExtraEffect =
    Involved -> Involved

type Action =
    {
        Suit : Suit
        Speed : int<speed>
        Damage : int<damage>
        Effect : ExtraEffect
        Originator : PlayerName
    }

type Phase =
    | Play
    | Respond of PlayerName * Action
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
    | Start of Initiative
    | InProgress of OpenGame
    | Complete of Winner

type Errors =
    | ``Playing cards you don't have`` of Player
    | ``Cannot play more than one card from your hand in a single action`` of Player
    | ``Knockdown can only be used with attack actions`` of Card list
    | ``Wrong suits`` of Card list
    | ``Cards do not form an action`` of Card list
    | ``Initiative phase has finished``
    | ``Invalid initiative card`` of PlayerName * Card
    | ``Playing not taking part in this game`` of PlayerName
    | ``Playing out of turn`` of PlayerName * Card list
    | ``Game is not in progress``
    | ``You can only put one card in your stance`` of Card list
    | ``Response is not a counter`` of Card list
    | ``Responses can only target the original attacker``
    | ``You cannot put cards in your stance outside your stance phase``
    | ``You cannot play an attack in your stance phase``
    | ``Unexpected error``