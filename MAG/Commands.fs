module MAG.Commands

open MAG

type Commands =
    | StartGame of System.Guid * PlayerConfig list
    | PlayInitiative of Player : PlayerName * Card
    | PlayAttack of Player : PlayerName * Target : PlayerName * Card list
    | Counter of Player : PlayerName * Card list
    | MoveToStance of Player : PlayerName * Card option
