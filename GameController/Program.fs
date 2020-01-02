// Learn more about F# at http://fsharp.org
module App

open System
open Akka.FSharp
open GameRoomParent
open Akka
open Akka.Actor

[<EntryPoint>]
let main argv =
    //root system actor
    use system = System.create "game-server-system" (Configuration.parse(Config.configString))

    [1..3]
    |> List.map (fun i -> ActorFactory.Factory.createActor system (sprintf "game%d" i |> Some) init update onOther)
    |> ignore

    while true do System.Threading.Thread.Sleep(1000000)

    0 // return an integer exit code
