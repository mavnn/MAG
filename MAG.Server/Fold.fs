module MAG.Server.Fold

open MAG
open MAG.Events
open MAG.Server.PlayerView
open Chessie.ErrorHandling

let playerViewFold player (state : CurrentState) (event : GameEvent) = 
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
                      Turn = None
                      WaitingFor = YouToPlayInitiative }
        | _ -> state
    | Played cm -> 
        match state with
        | OnGoing({ You = you; WaitingFor = YouToMoveStance } as og) when cm.Player = player -> 
            OnGoing { og with You = 
                                  { you with Hand = you.Hand |> List.filter (fun c -> c <> cm.Card)
                                             Stance = cm.Card :: you.Stance } }
        | OnGoing({ You = you } as og) when cm.Player = player -> 
            OnGoing { og with You = 
                                  { you with Hand = you.Hand |> List.filter (fun c -> c <> cm.Card)
                                             Stance = you.Stance |> List.filter (fun c -> c <> cm.Card)
                                             Discards = cm.Card :: you.Discards } }
        | OnGoing({ Them = them; WaitingFor = Them(t, Some cs) } as og) -> 
            let them' = 
                them |> List.map (fun t -> 
                            if t.Name = cm.Player then 
                                { t with Stance = t.Stance |> List.filter (fun c -> c <> cm.Card)
                                         Discards = cm.Card :: t.Discards }
                            else t)
            OnGoing { og with Them = them'; WaitingFor = Them(t, Some (cm.Card::cs)) }
        | OnGoing({ Them = them; WaitingFor = Them(_) } as og) -> 
            let them' = 
                them |> List.map (fun t -> 
                            if t.Name = cm.Player then 
                                { t with Stance = t.Stance |> List.filter (fun c -> c <> cm.Card)
                                         Discards = cm.Card :: t.Discards }
                            else t)
            OnGoing { og with Them = them' }
        | OnGoing({ Them = them; WaitingFor = ThemToMoveStance(_) } as og) -> 
            let them' = 
                them |> List.map (fun t -> 
                            if t.Name = cm.Player then 
                                { t with Stance = cm.Card::t.Stance }
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
        | OnGoing({ You = you; Them = them; Turn = Some t } as og) when dt.Player = you.Name -> 
            let next =
                if t = player then
                    YouToMoveStance
                else
                    Them (t, None)
            OnGoing { og with You = { you with Life = you.Life - dt.Damage }; WaitingFor = next }
        | OnGoing({ You = you; Them = them; Turn = Some t } as og) when dt.Player <> you.Name -> 
            let them' = 
                them |> List.map (fun t -> 
                            if t.Name = dt.Player then { t with Life = t.Life - dt.Damage }
                            else t)
            let next =
                if t = player then
                    YouToMoveStance
                else
                    Them (t, None)
            OnGoing { og with Them = them'; WaitingFor = next }
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
    | Target t ->
        match state with
        | OnGoing ({ WaitingFor = YouToAttack None } as og) ->
            OnGoing { og with WaitingFor = YouToAttack (Some t.Target) }
        | OnGoing ({ WaitingFor = Them (them, None) } as og) when t.Target = player ->
            OnGoing { og with WaitingFor = Them (them, Some []) }
        | _ ->
            state
    | PhaseComplete _ ->
        match state with
        | OnGoing ({ WaitingFor = YouToCounter _; Turn = Some t } as og) when t <> player ->
            OnGoing { og with WaitingFor = ThemToMoveStance t }
        | OnGoing ({ WaitingFor = YouToAttack (Some t) } as og) ->
            OnGoing { og with WaitingFor = Them (t, None) }
        | OnGoing ({ WaitingFor = Them (t, Some ((_::_) as cards))} as og) ->
            OnGoing { og with WaitingFor = YouToCounter(t, cards)}
        | OnGoing ({ Turn = Some t } as og) when t = player ->
            OnGoing { og with WaitingFor = YouToMoveStance }
        | _ ->
            state
    | TurnStarted { Player = newPlayer } ->
        match state with
        | OnGoing og ->
            if newPlayer = player then
                OnGoing { og with
                            WaitingFor = YouToAttack None
                            Turn = Some newPlayer }
            else
                OnGoing { og with
                            WaitingFor = Them (newPlayer, None)
                            Turn = Some newPlayer }
        | _ ->
            state
    | CommandRecieved _ -> state

let gameFold result event =
    result
    |> bind (fun g ->
        GameEvents.processEvent g event)

