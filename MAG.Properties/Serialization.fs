module MAG.Properties.Serilization

open Chiron
open FsCheck
open FsCheck.PropOperators
open MAG
open NUnit.Framework

type SerializationProperties =
    static member ``Cards can be round tripped to JSON`` (s : Card) =
        let thing =
            Json.serialize s
            |> Json.format
        let (thing2 : Card) =
            (Json.deserialize << Json.parse) thing
        thing2 = s |@ sprintf "%A = %A" thing2 s
    static member ``GameEvent can be round tripped to JSON`` (s : GameEvent) =
        let thing =
            Json.serialize s
            |> Json.format
        let (thing2 : GameEvent) =
            (Json.deserialize << Json.parse) thing
        thing2 = s |@ sprintf "%A = %A" thing2 s

type SafeString =
    static member SafeString () =
        Arb.Default.String()
        |> Arb.filter (fun s -> s <> null)

[<Test>]
let ``Serilization properties`` () =
    Check.All<SerializationProperties>({ Config.Verbose with Arbitrary = [typeof<SafeString>] })