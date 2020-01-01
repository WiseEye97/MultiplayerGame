module MainActor

open Shared4
open Shared4.LauncherMsg
open Akka.FSharp
open Elmish
open Common
open Akka.Actor
open System.IO

type MenuActor = MenuActor of IActorRef
type LobbyActor = LobbyActor of IActorRef
type GameActor = GameActor of IActorRef

type Model =
    | Initial
    | Ready of MenuActor
    | InLobby of LobbyActor*MenuActor


let initial (self : IActorRef) =
    self <! (SystemMsg SendAck)
    Initial
   
let private connectionString = ActorPath.Parse("akka.tcp://menu-server-system@192.168.0.105:8080/user/test1")

let sendToActor (actor : IActorRef) (msg : MenuServerMsg.MainMenuMsgs) = actor <! (SystemMsg (SendToServer msg));actor

let actorWrapper (actor : IActorRef) = {
    actorRef = actor
    dispatch = fun (msg : MenuServerMsg.FromUser) -> actor <! (msg |> MenuServerMsg.MainMenuMsgs.FromUser |> SendToServer |> SystemMsg)
}

let actorLobbyWrapper (actor : IActorRef) = {
    actorRef = actor
    dispatch = fun (msg : LobbyMsg.FromUser) -> actor <!  (SystemMsg (SendToLobby msg))
}

let update (mailbox : Actor<obj>) msg model (dispatch : Dispatch<WPFMsg>) =
    
    match model,msg with
    | Initial,SystemMsg SendAck ->
        let menuActor = async { 
            let! rf = mailbox.Context.ActorSelection(connectionString).ResolveOne(System.TimeSpan.FromSeconds(5.0)) |> Async.AwaitTask
            return MenuActor rf
        } 
        
        let menuActor = menuActor |> Async.RunSynchronously
        
        let self = mailbox.Self
        dispatch <| RegisterActor self
        Ready menuActor

    | Ready (MenuActor menuActor),SystemMsg (SendToServer msg ) -> menuActor <! msg;model
    | Ready (MenuActor menuActor),MsgToUi msg when mailbox.Sender() = menuActor ->
        dispatch <| FromServer msg
        model
    | Ready (MenuActor menuActor as ma),MsgToUi (Confirmation (JoinedLobby info)) when mailbox.Sender() <> menuActor ->
        
        let lobbyRef = mailbox.Sender()

        info
        |> JoinedLobby
        |> Confirmation
        |> FromServer
        |> dispatch
        
        InLobby ((LobbyActor lobbyRef , ma))
    | Ready x,SystemMsg (AddLobbyActor lobbyActor) -> InLobby ((LobbyActor lobbyActor,x))
    | InLobby ((LobbyActor _),_),SystemMsg (InitGameActor info) ->
        
        let tryResolve (path : ActorPath) = 
            async {
                let! r = mailbox.Context.ActorSelection(path).ResolveOne(System.TimeSpan.FromSeconds(5.0)) |> Async.AwaitTask |> Async.Catch
                let x = match r with
                        | Choice1Of2 c -> Some c
                        | _ -> None
                return x
            }
             
        let resolver = 
            let portToBind = 1
            fun () ->
                let cmp = 
                    [for i in portToBind..portToBind -> ActorPath.Parse(sprintf "akka.tcp://godot-system@localhost:809%d/user/gameactor" i)]
                    |> List.map tryResolve
                    |> Async.Choice
                async {
                    let! x = cmp
                    return x.Value
                }
                

        {token = info.token;gameActor = resolver;which = info.which} 
        |> StartGameRef 
        |> LobbyNotification 
        |> Notification 
        |> FromServer
        |> dispatch

        model
    | InLobby ((LobbyActor lobbyActor),_),SystemMsg (SendToLobby msg) -> lobbyActor <! (Shared4.LobbyMsg.FromUser msg);model

    | InLobby ((LobbyActor lobbyActor),_),MsgToUi msg when mailbox.Sender() = lobbyActor ->
        dispatch <| FromServer msg
        model
    | InLobby ((LobbyActor _),(MenuActor menuActor as ma)),MsgToUi (Confirmation (BackToMenu _) as msg) when mailbox.Sender() = menuActor ->
        dispatch <| FromServer msg
        Ready ma
    
        

