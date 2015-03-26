module MAG.Server.Fold

open MAG
open MAG.Events
open MAG.Server.PlayerView

let EventFold player (state : CurrentState) (event : GameEvent) = 
    match event with
    | Created c -> 
        match state with
        | Nothing -> 
            let you, them = 
                c.Players
                |> List.map (fun p -> p.Name)
                |> List.partition (fun p -> p = player)
            OnGoing { You = 
                          { Name = player
                            Life = 20
                            Hand = []
                            Stance = []
                            Discards = [] }
                      Them = 
                          them |> List.map (fun t -> 
                                      { Name = t
                                        Life = 20
                                        Stance = []
                                        Discards = [] })
                      WaitingFor = YouToPlayInitiative }
        | _ -> state
    | Played cm -> 
        match state with
        | OnGoing({ You = you; Them = them } as og) -> 
            if cm.Player = you.Name then 
                OnGoing { og with You = 
                                      { you with Hand = you.Hand |> List.filter (fun c -> c <> cm.Card)
                                                 Stance = you.Stance |> List.filter (fun c -> c <> cm.Card)
                                                 Discards = cm.Card :: you.Discards } }
            else 
                let them' = 
                    them |> List.map (fun t -> 
                                if t.Name = cm.Player then 
                                    { t with Stance = t.Stance |> List.filter (fun c -> c <> cm.Card)
                                             Discards = cm.Card :: t.Discards }
                                else t)
                OnGoing { og with Them = them' }
        | _ -> state
    | Drawn cm -> 
        match state with
        | OnGoing({ You = you; Them = them } as og) when you.Name = cm.Player -> 
            OnGoing { og with You = { you with Hand = cm.Card :: you.Hand } }
        | _ -> state
    | DamageTaken dt -> 
        match state with
        | OnGoing({ You = you; Them = them } as og) -> 
            if dt.Player = you.Name then OnGoing { og with You = { you with Life = you.Life - dt.Damage } }
            else 
                let them' = 
                    them |> List.map (fun t -> 
                                if t.Name = dt.Player then { t with Life = t.Life - dt.Damage }
                                else t)
                OnGoing { og with Them = them' }
        | _ -> state
    | KnockedDown kd -> 
        match state with
        | OnGoing({ You = you; Them = them } as og) -> 
            if kd.Target = you.Name then 
                OnGoing { og with You = 
                                      { you with Stance = []
                                                 Discards = List.concat [ you.Stance; you.Discards ] } }
            else 
                let them' = 
                    them |> List.map (fun t -> 
                                if t.Name = kd.Target then 
                                    { t with Stance = []
                                             Discards = List.concat [ t.Stance; t.Discards ] }
                                else t)
                OnGoing { og with Them = them' }
        | _ -> state
    | Target _ -> state
    | PhaseComplete(_) -> failwith "Not implemented yet"
    | CommandRecieved _ -> state
