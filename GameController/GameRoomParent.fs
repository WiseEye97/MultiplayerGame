module GameRoomParent

open Shared4.GameRoomMsg
open Akka.FSharp
open Akka.Actor
open System

type GameRoomMsgTp = SharedTypes.GameServerTypes.GameNotification

type Model = {
    rooms : Map<Guid,Shared4.Common.ActorRefWrapper<GameRoomMsgTp>>
    pendingRequests : IActorRef list
}

let init = 
    {
        rooms = Map.empty
        pendingRequests = []
    }

let update (actor : Actor<obj>) (msg : ParentGameRoomMsg) (model : Model) =
    let sender = actor.Sender()

    let inline calculateWeight () = 
        model.rooms 
        |> Seq.length 
        |> Shared4.LobbyMsg.Weight 
        |> Shared4.LobbyMsg.FromGameRoomParent
        
    match msg with
    | AskForWeight ->
        let weight = calculateWeight ()
        sender <! weight
        model
    | RoomRequest ->  
        {model with pendingRequests = sender :: model.pendingRequests}     
    | ParentGameRoomMsg.StartGame ->
        let newPendings = model.pendingRequests |> List.filter ((<>) sender)

        let roomGuid = Guid.NewGuid()

        let room = 
            ActorFactory.Factory.createActor 
                actor.Context 
                None 
                GameRoom.init 
                GameRoom.update ActorFactory.Factory.emptyOther 
            |> fun r -> {Shared4.Common.actorRef = r;Shared4.Common.dispatch = fun (m : GameRoomMsgTp) -> r <! m}

        let msgResp = Shared4.LobbyMsg.makeLobbyRefStr <| roomGuid.ToString()
        sender <! msgResp

        {model with rooms = Map.add roomGuid room model.rooms;pendingRequests = newPendings}
 
let onOther (actor : Actor<obj>) (msg : obj) (model : Model) =
    match msg with
    | :? SharedTypes.GameTypes.RegisterInLobby as reg ->
        let sender = actor.Sender()
        let lobby = model.rooms.[Guid.Parse(reg.token)]
        let response = SharedTypes.GameTypes.Msg.LobbyResolved (lobby.actorRef |> Akka.Serialization.Serialization.SerializedActorPath,reg.name)
        sender <! response
    | _ -> ()
    model

