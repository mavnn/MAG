module MAG.Server.Suave

open MAG
open MAG.Server
open MAG.Server.UI
open Suave
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Types
open Suave.Web
open Suave.Utils

let parseUser (ctx : HttpContext) =
    let header = ctx.request.header "authorization"
    match header with
    | Some token ->
        let parts = token.Split (' ')
        let enc = parts.[1].Trim()
        let decoded = ASCII.decodeBase64 enc
        let indexOfColon = decoded.IndexOf(':')
        decoded.Substring(0,indexOfColon)
    | None ->
        ""

let basicAuth =
    fun (n, _) -> true
    |> Authentication.authenticateBasic

let app =
    choose [
        basicAuth
        (warbler <| fun ctx -> Writers.setUserData "userName" (parseUser ctx))
        >>= choose [
                path "/" >>= (context (fun c -> OK (html <| unbox c.userState.["userName"])))
                pathScan "/game/%s" <| fun gid -> OK (sprintf "%A" (EventStore.reloaded gid))
                pathScan "/player/%s" <| fun name -> OK (sprintf "%A" (PlayerName name))
            ]
    ]

let cts = new System.Threading.CancellationTokenSource()
let listening, server = startWebServerAsync defaultConfig app
Async.Start(server, cts.Token)

listening |> Async.RunSynchronously |> printfn "start stats: %A"

System.Console.ReadLine() |> ignore

cts.Cancel()

let g = "26bc09f9-4585-45b3-a1bd-d821b644e67d"