module ActorUtil

open Akka.Actor
open Akka.FSharp
open Shared4
open Shared4.LauncherMsg

let sendToActor (actor : IActorRef) (msg : MenuServerMsg.MainMenuMsgs)  = actor <! (SystemMsg (SendToServer msg));actor
