module MAG.Properties.Serilization

open Chiron
open FsCheck
open FsCheck.PropOperators
open MAG
open NUnit.Framework

let inline roundTrip (thing : 'a) : 'a =
    Json.serialize thing
    |> Json.format
    |> Json.parse
    |> Json.deserialize

type EventSerializationProperties =
    static member ``GameEvent can be round tripped to JSON`` (s : GameEvent) =
        let sut = roundTrip s
        sut = s |@ sprintf "(%A should equal %A)" sut s

type PlayerViewSerializationProperties =
    static member ``CurrentState can be round tripped to JSON`` (s : MAG.Server.PlayerView.CurrentState) =
        let sut = roundTrip s
        sut = s |@ sprintf "(%A should equal %A)" sut s

type SafeString =
    static member SafeString () =
        Arb.Default.String()
        |> Arb.filter (fun s ->
            s <> null)

[<Test>]
let ``Event Serialization properties`` () =
    Check.All<EventSerializationProperties>({ Config.VerboseThrowOnFailure with Arbitrary = [typeof<SafeString>] })

[<Test>]
let ``PlayerView Serialization properties`` () =
    Check.All<PlayerViewSerializationProperties>({ Config.VerboseThrowOnFailure with Arbitrary = [typeof<SafeString>] })
