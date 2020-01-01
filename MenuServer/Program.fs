// Learn more about F# at http://fsharp.org

open System
open Akka.FSharp
open Akka.Actor
open Shared4.MenuServerMsg
open ActorFactory
open System.Text


[<EntryPoint>]
let main argv =
    
    use system = System.create "menu-server-system" (Configuration.parse(configString))
    
    let _mainActor = MainActor.myActor system
   
    while true do 
        System.Threading.Thread.Sleep(500000)

    0 // return an integer exit code
