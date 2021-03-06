﻿module MAG.Action

open System
open Chessie.ErrorHandling
open MAG

let isCounter attack counter =
    match attack, counter with
    | { Speed = attackSpeed }, { Suit = Defend; Speed = counterSpeed }
    | { Suit = Punch; Speed = attackSpeed }, { Suit = Kick; Speed = counterSpeed }
    | { Suit = Kick; Speed = attackSpeed }, { Suit = Throw; Speed = counterSpeed }
    | { Suit = Throw; Speed = attackSpeed }, { Suit = Punch; Speed = counterSpeed }
            when counterSpeed <= attackSpeed ->
        true
    | _ ->
        false
