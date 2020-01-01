module Lobby

open Elmish
open Elmish.WPF
open Shared4.LauncherMsg
open Shared4.Common
open Shared4.LobbyMsg
open Akka.FSharp
open System
open System.Diagnostics

type UserMsg = {
    nick : string
    msg : string
}

type LobbyChatMsg = 
    | SystemMsg of string
    | UserMsg of UserMsg
    with 
        member this.GetContent() = 
            match this with
            | SystemMsg s -> s
            | UserMsg u -> sprintf "%s : %s " u.nick u.msg

type Confirmations = 
    | MessageDeliveried

type Msg = 
    | ServerNotifications of Shared4.LauncherMsg.LobbyNotification
    | ServerConfirmations of Confirmations
    | ChatMsgChanged of string
    | EmitChatMsg
    | GoReady
    | GoUnReady
    | Leave

type LobbyClientModel = {
    status : UserLobbyStatus
    lobbyActor : ActorRefWrapper<FromUser>
    msgs : (LobbyChatMsg*Guid) list
    users : LobbyUserInfo list
    currentMsg : string
    myName : string
}

let update (msg : Msg) (model : LobbyClientModel) =
    let handleNotification = 
        function
        | StartGameRef info ->
            let startInfo = new ProcessStartInfo();

            startInfo.CreateNoWindow <- false
            startInfo.UseShellExecute <- false
            startInfo.FileName <- """F:\GAME_EXPORT_TEST\game.exe"""
            startInfo.WindowStyle <- ProcessWindowStyle.Hidden

            let initMsg = {SharedTypes.GameTypes.serverIndex = info.which.ToInt(); SharedTypes.GameTypes.lobbyToken = info.token;SharedTypes.GameTypes.myName = model.myName} |> SharedTypes.GameTypes.InitialFromLauncher

            let timer = new System.Timers.Timer(5000.0)
            timer.AutoReset <- true
            timer.Elapsed.Add(fun _x -> () |> info.gameActor |> Async.RunSynchronously |> fun r -> r <! initMsg;timer.Stop())
            timer.Start()

            try
                use exeProcess = Process.Start(startInfo)
                exeProcess.WaitForExit()
            with
            | ex ->
                printfn "Exception During Execution %A" ex
                ()

            model,Cmd.none
        | NewUserJoined userInfo ->
            let newSystemMsg = sprintf "%s has joined" userInfo.nick
            {model with users = userInfo :: model.users;msgs = ((SystemMsg newSystemMsg),Guid.NewGuid()) :: model.msgs},Cmd.none
        | LobbyNotification.PlayerReady userInfo ->
            let newUsers = model.users |> List.map (function | x when x.playerId.Equals(userInfo.playerId) -> userInfo | x -> x)
            {model with users = newUsers},Cmd.none
        | LobbyNotification.PlayerUnReady userInfo ->
            let newUsers = model.users |> List.map (function | x when x.playerId.Equals(userInfo.playerId) -> userInfo | x -> x)
            {model with users = newUsers},Cmd.none
        | UserLeft userInfo ->
            let newSystemMsg = sprintf "%s has left" userInfo.nick
            let newUsers = model.users |> List.filter (function | x -> not <| x.playerId.Equals(userInfo.playerId))
            {model with users = newUsers;msgs = ((SystemMsg newSystemMsg),Guid.NewGuid()) :: model.msgs},Cmd.none
        | NewUserChatMsg (userInfo,userMsg) ->
            {model with msgs = ((UserMsg {nick = userInfo.nick;msg = userMsg}),Guid.NewGuid()) :: model.msgs},Cmd.none
        | GameStarting ->
            model,Cmd.none
    
    let handleConfirmation = 
        function
        | MessageDeliveried ->
            let myChatMsg = {nick = "Ja";msg = model.currentMsg}
            {model with currentMsg = "";msgs = ((UserMsg myChatMsg),Guid.NewGuid()) :: model.msgs},Cmd.none

    let changeMyStatus newStat = 
        {model with users = model.users |> List.map (function | x when x.nick = model.myName -> {x with status = newStat} | x -> x)}

    match msg with
    | Leave ->
        model.lobbyActor.dispatch PlayerLeft
        model,Cmd.none
    | GoReady -> 
        model.lobbyActor.dispatch PlayerReady
        {changeMyStatus Ready with status = Ready},Cmd.none
    | GoUnReady ->
        model.lobbyActor.dispatch PlayerUnReady
        {changeMyStatus UnReady with status = UnReady},Cmd.none
    | EmitChatMsg ->

        model.currentMsg 
        |> PlayerChatMsg 
        |> model.lobbyActor.dispatch
         
        model,Cmd.none 
    | ChatMsgChanged newChatMsg -> 
        {model with currentMsg = newChatMsg},Cmd.none
    | ServerNotifications n -> handleNotification n
    | ServerConfirmations c -> handleConfirmation c
 
    

let init (_,users,actorWrapper,myName) =
    
    let me = {
        nick = myName
        playerId = Guid.Empty // ??? TODO
        status = UnReady
    }

    let initModel = 
        {   
            status = UnReady
            msgs = []
            lobbyActor = actorWrapper
            users = me :: users
            currentMsg = ""
            myName = myName
        }

    initModel,Cmd.none

let playerBindings () : Binding<LobbyClientModel*LobbyUserInfo,Msg> list = [
    "Name" |> Binding.oneWay (function _,{LobbyUserInfo.nick = n} -> n)
    "Status" |> Binding.oneWay (function _,x -> x.status.ToStr())
]

let messageBindings () : Binding<LobbyClientModel*(LobbyChatMsg*Guid),Msg> list = [
    "Content" |> Binding.oneWay (function | _,(m,_) -> m.GetContent())
]

let bindings () : Binding<LobbyClientModel,Msg> list = [
    "Players" |> Binding.subModelSeq((function | {LobbyClientModel.users = u} -> u),(function | {LobbyUserInfo.playerId = p} -> p),playerBindings)
    "MessageContent" |> Binding.twoWay((function | {currentMsg = cmsg} -> cmsg),(fun newVal _ -> ChatMsgChanged newVal))
    "SendMessage" |> Binding.cmd EmitChatMsg
    "ReadyUp" |> Binding.cmd GoReady
    "UnReady" |> Binding.cmd GoUnReady
    "CanReadyUp" |> Binding.oneWay ((function | {status = s} -> s = UnReady))
    "CanUnReady" |> Binding.oneWay ((function | {status = s} -> s = Ready))
    "ChatMessages" |> Binding.subModelSeq((function | {LobbyClientModel.msgs = u} -> u),(function | (_,gid) -> gid),messageBindings)
    "Leave" |> Binding.cmd Leave
]
