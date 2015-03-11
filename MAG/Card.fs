[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MAG.Card

open System
open MAG
open Chessie.ErrorHandling

let rec private insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec private permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let private knockdown involved =
    { involved with
        Defender = {
                    involved.Defender with
                        Stance = []
                        Discards =
                            List.concat [
                                involved.Defender.Stance
                                involved.Defender.Discards] } }

let private safeReduce cards reduce xs =
    trial {
        match xs with
        | [] -> return! fail <| ``Cards do not form an action`` cards
        | _ -> return List.reduce reduce xs
    }

let rec private multiCardAttack playerName spdSuit secondSuit damFilter spdReduce damReduce cards =
    trial {
        let! attackCards =
            match cards with
            | [] -> fail <| ``Cards do not form an action`` cards
            | _::t ->
                match List.length t with
                | 2 -> ok t
                | _ -> fail <| ``Cards do not form an action`` cards
        let! attacks =
            attackCards
            |> List.map (fun c -> toAction playerName [c])
            |> collect
            |> bind (fun r -> List.ofSeq r |> ok)
        let! speed =
            attacks
            |> List.filter (fun { Suit = s } -> spdSuit = s)
            |> List.map (fun { Speed = s } -> s)
            |> safeReduce cards spdReduce
        let! damage =
            attacks
            |> List.filter damFilter
            |> List.map (fun { Damage = d } -> d)
            |> safeReduce cards damReduce
        match speed, damage with
        | 0<speed>, _
        | _, 0<damage> ->
            return! fail (``Wrong suits`` cards)
        | _ when attacks |> List.map (fun { Suit = s } -> s) |> List.sort = List.sort[spdSuit;secondSuit] ->
            return { Suit = spdSuit; Speed = speed; Damage = damage; Effect = id; Originator = playerName }
        | _ ->
            return! fail (``Wrong suits`` cards) }

and private combo playerName openerSuit comboSuit cards =
    let damFilter =
        fun _ -> true
    multiCardAttack playerName openerSuit comboSuit damFilter min (+) cards

and private mega playerName spdSuit damSuit cards =
    let damFilter =
        fun { Suit = s } -> s = damSuit
    multiCardAttack playerName spdSuit damSuit damFilter min max cards

and toAction playerName cards =
    trial {
        let sorted = cards |> List.sort
        match sorted with
        | KnockDown::t::[] ->
            let! a = toAction playerName [t]
            match a with
            | { Suit = Punch }
            | { Suit = Kick }
            | { Suit = Throw } ->
                return { a with Effect = a.Effect >> knockdown }
            | _ ->
                return! fail (``Knockdown can only be used with attack actions`` sorted)
        | Combo (spdSuit, damSuit)::first::second::[] ->
            return! combo playerName spdSuit damSuit sorted
        | MegaAttack (name, spdSuit, damSuit)::first::second::[] ->
            return! mega playerName spdSuit damSuit sorted
        | [Basic (Defend, b)] ->
            return { Suit = Defend; Speed = 1<speed> * int b; Damage = 0<damage>; Effect = id; Originator = playerName }
        | [Basic (suit, b)] ->
            return { Suit = suit; Speed = 1<speed> * int b; Damage = 1<damage> * int b; Effect = id; Originator = playerName }
        | _ ->
            return! fail (``Cards do not form an action`` sorted)
    }
