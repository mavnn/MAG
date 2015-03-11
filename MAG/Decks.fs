module MAG.Decks

open MAG

let Decks =
    [
        DeckName "Soo Bank Do",
        [
            [ MegaAttack ("Earth Sink", Defend, Punch) ]
            [ Combo (Punch, Kick)]
            [ KnockDown ]
            [ for i in 1 .. 5 -> Basic(Punch, i) ]
            [ for i in 2 .. 6 -> Basic(Kick, i) ]
            [ for i in 5 .. 6 -> Basic(Throw, i) ]
            [ for i in 1 .. 5 -> Basic(Defend, i) ]
        ] |> List.concat

        DeckName "Jiu Jitsu",
        [
            [ MegaAttack ("Mega Throw", Throw, Throw) ]
            [ Combo (Punch, Throw)]
            [ KnockDown ]
            [ for i in 2 .. 4 -> Basic(Punch, i) ]
            [ for i in 3 .. 5 -> Basic(Kick, i) ]
            [ for i in 2 .. 7 -> Basic(Throw, i) ]
            [ for i in 1 .. 5 -> Basic(Defend, i) ]
        ] |> List.concat
    ] |> Map.ofList
