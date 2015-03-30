
type Addition<'a> =
    | Add of 'a * 'a
    | Const of 'a

type Mult<'a> =
    | Mult of 'a *'a

let (|Add'|Const'|Mult'|) x =
    match x with
    | Choice1Of2 (Add (a, a')) -> Add'(a, a')
    | Choice1Of2 (Const a) -> Const' a
    | Choice2Of2 (Mult (a, a')) -> Mult' (a, a')
        
        