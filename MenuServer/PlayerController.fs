module PlayerController

open Akka.FSharp

let sendMsgToPlayer playerRef (msg: Shared4.LauncherMsg.LauncherMsgs) = playerRef <! msg
