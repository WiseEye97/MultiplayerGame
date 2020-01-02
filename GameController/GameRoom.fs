module GameRoom

open Akka.Actor
open Akka.FSharp
open SharedTypes.GameServerTypes
open System
open Shared4.Common

type Player = {
    pos : float32*float32
    name : string
    dispatch : ActorRefWrapper<SharedTypes.GameTypes.GameCommands>
}
    with
        member this.ToInfo() = {
            SharedTypes.GameTypes.OpponentInfo.pos = this.pos
            SharedTypes.GameTypes.OpponentInfo.name = this.name}
module Player = 

    let getRef {Player.dispatch = {actorRef = d}} = d

let private playerDispatch rf (d : SharedTypes.GameTypes.GameCommands) = rf <! d

type Model = {
    players : Player list
}

let init = {players = []}

let update (actor : Actor<obj>) (msg : SharedTypes.GameServerTypes.GameNotification) (model : Model) =
    
    let generateNewSpawnPoint () = 
        let r = System.Random()

        (r.Next(50) |> float32,r.Next(50) |> float32)

    let sender = actor.Sender()

    match msg with
    | RegisterPlayer str ->
        let player = {
            pos = generateNewSpawnPoint ()
            name = str
            dispatch = {dispatch = playerDispatch sender;actorRef = sender}
        }

        let msgToSend = player.ToInfo() |> SharedTypes.GameTypes.SpawnOpponent

        model.players
        |> Seq.map (fun x -> x.dispatch.dispatch)
        |> Seq.iter ((|>) msgToSend)

        {model with players = player :: model.players}
    | MoveEvent ev ->
        let toSend = 
            model.players 
            |> Seq.find (Player.getRef >> (=) sender)
            |> fun p -> p.ToInfo(),ev
            |> SharedTypes.GameTypes.OpponentMove

        model.players
        |> Seq.filter (Player.getRef >> (<>) sender)
        |> Seq.iter (fun p -> p.dispatch.dispatch toSend)

        model