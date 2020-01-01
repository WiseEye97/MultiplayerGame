module ActorUtil

open Akka.Actor
open Akka.FSharp
open Shared4.LauncherMsg

let sendToUser (actor : IActorRef) (msg : Shared4.LauncherMsg.MsgToUi) = actor <! (MsgToUi msg);actor

let sendToLobby (actor : IActorRef) (msg : Shared4.LobbyMsg.LobbyMsg) = actor <! msg