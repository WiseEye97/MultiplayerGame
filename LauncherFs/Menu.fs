module Menu

open Elmish
open Elmish.WPF
open Shared4.LauncherMsg
open Shared4.Common
open System
open System.Windows

module LobbyCreator =

    type LobbyName = LobbyName of string

    type LobbySize = private LobbySize of int
        with
            static member FromInt = 
                function
                | x when x > 0 && x < 17 -> x |> LobbySize |> Some
                | _ -> None

            static member FromComboIndex = 
                function
                | 0 -> LobbySize.FromInt 4
                | 1 -> LobbySize.FromInt 8
                | 2 -> LobbySize.FromInt 16
                | _ -> None

            member this.Get() = match this with LobbySize s -> s

            member this.ToIndex() = 
                let (LobbySize s) = this
                match s with 
                | 4 -> 0
                | 8 -> 1
                | 16 -> 2
                | _ -> failwith ""
    
    type Msg = 
        | CreateCommand
        | NameChanged of LobbyName
        | SizeChanged of LobbySize
    
    type CreatorStatus = 
        | StandBy
        | Creating

    type Model = {
        LobbyName : LobbyName
        LobbySize : LobbySize
        Status : CreatorStatus
    } 
        with
            member this.GetStatusText() = 
                match this.Status with
                | StandBy -> ""
                | Creating -> "Tworzenie Lobby"

            member this.CanClick() = this.Status = StandBy
    
    let update msg model : (Model*Cmd<Msg>)= 
        match msg with
        | NameChanged newName -> {model with LobbyName = newName},Cmd.none
        | SizeChanged newSize -> {model with LobbySize = newSize},Cmd.none
        | _ -> failwith ""

    let init() : Model*Cmd<Msg> = ({
            LobbyName = LobbyName ""
            LobbySize = LobbySize.FromInt 8 |> Option.get
            Status = CreatorStatus.StandBy
        },Cmd.none)

type MenuStatus = 
    | Idle
    | WaitingForLobbyCreation

open Shared4.MenuServerMsg

type Model = {
    GameList : ServerGameInfo list
    ChatMsgs : ChatMessageInfo list
    PendingMsg : string option
    ChatMsg : string
    CreateLobbyWindow : LobbyCreator.Model option
    ActorRef : ActorRefWrapper<FromUser>
    MenuStatus : MenuStatus
}

type UserName = UserName of string
type LobbyId = LobbyId of Guid

type ConfirmationFromServer = 
    | MessageSend of UserName

type NotificationsFromServer = 
    | AddNewLobby of Shared4.Common.ServerGameInfo
    | NewChatMsg of ChatMessageInfo
    | ServerEdited of Shared4.Common.ServerGameInfo

let toNewChatMsg = NewChatMsg

type Msg = 
    | JoinLobby of LobbyId
    | RedirectToUser of UserName
    | CreateLobby
    | EmitChatMsg
    | ChatMsgChanged of string
    | CreateLobbyMsg of LobbyCreator.Msg
    | NotificationsFromServer of NotificationsFromServer
    | ConfirmationFromServer of ConfirmationFromServer
    | UserMenuMsg of UserMenu.Msg

let init ({ServerList = games;ChatMsgs = msgs},rf) : Model*Cmd<Msg> = 
    {GameList = games;ChatMsgs = msgs;ChatMsg = "";CreateLobbyWindow = None;ActorRef = rf;MenuStatus = MenuStatus.Idle;PendingMsg = None},Cmd.none

let update msg model =

    let handleFromNetwork =
        function
        | AddNewLobby gameInfo -> 
            {model with GameList = gameInfo :: model.GameList},Cmd.none
        | NewChatMsg chatMsg ->
            {model with ChatMsgs = chatMsg :: model.ChatMsgs},Cmd.none
        | ServerEdited editedInfo ->
            let newGameList = model.GameList |> List.map (fun game -> if game.LobbyId = editedInfo.LobbyId then editedInfo else game)
            {model with GameList = newGameList},Cmd.none
    
    let handlerConfirmations = 
        function
        | MessageSend (UserName myName),{PendingMsg = Some msg} ->
            {model with ChatMsgs = {senderName = myName;content = msg} :: model.ChatMsgs},Cmd.none
        | MessageSend (UserName _),{PendingMsg = None} -> 
            failwith "No pedning message"
         
     
    match msg with
    | UserMenuMsg m ->
        
        failwith ""
    | NotificationsFromServer notif ->
        handleFromNetwork notif
    | ConfirmationFromServer conf ->
        handlerConfirmations (conf,model)
    | JoinLobby (LobbyId lobbyId) ->
        model.ActorRef.dispatch (Shared4.MenuServerMsg.JoinServer lobbyId)
        model,Cmd.none
    | CreateLobbyMsg (LobbyCreator.CreateCommand) ->

        let {LobbyCreator.Model.LobbyName = LobbyCreator.LobbyName ln;LobbyCreator.Model.LobbySize = size} as creatorModel = model.CreateLobbyWindow |> Option.get

        let reqModel = {
            Shared4.MenuServerMsg.CreateLobby.serverName = ln
            Shared4.MenuServerMsg.CreateLobby.roomSize = size.Get()
        }

        model.ActorRef.dispatch (Shared4.MenuServerMsg.CreateLobby reqModel) 
       
        {model with MenuStatus = WaitingForLobbyCreation; CreateLobbyWindow = Some {creatorModel with Status = LobbyCreator.Creating}},Cmd.none
    | CreateLobbyMsg msg ->

        let creatorModel = model.CreateLobbyWindow |> Option.get
        let (newModel,lobbyCCmd) = LobbyCreator.update msg creatorModel
        {model with CreateLobbyWindow = Some newModel},Cmd.map CreateLobbyMsg lobbyCCmd

    | ChatMsgChanged newMsg -> {model with ChatMsg = newMsg},Cmd.none
    | EmitChatMsg ->

         model.ChatMsg
         |> Shared4.MenuServerMsg.MenuChatMsg
         |> model.ActorRef.dispatch

         {model with ChatMsg = "";PendingMsg = Some model.ChatMsg},Cmd.none
    | CreateLobby ->
        let (creatorWindow,cmd) = LobbyCreator.init()
        {model with CreateLobbyWindow = Some creatorWindow},Cmd.map CreateLobbyMsg cmd

    | RedirectToUser (UserName _) -> raise(NotImplementedException())
    
let serverInfoBindings () : Binding<Model*ServerGameInfo,Msg> list  = [
    "ServerName" |> Binding.oneWay (function _,{ServerGameInfo.Name = n} -> n)
    "Size" |> Binding.oneWay (function _,{ServerGameInfo.Size = n;ServerGameInfo.PlayersCount = c} -> sprintf "%d/%d" c n)
    "Status" |> Binding.oneWay (function _,{ServerGameInfo.Status = n} -> n)
    "IsActive" |> Binding.oneWay (function _,{ServerGameInfo.Status = _n} -> true)
    "JoinLobby" |> Binding.cmd (fun (_,model) -> model.LobbyId |> LobbyId |> JoinLobby)
]

let chatMsgBindings () : Binding<Model*Shared4.LauncherMsg.ChatMessageInfo,Msg> list = [
    "Author" |> Binding.oneWay (function _,{senderName = n} -> n)
    "Content" |> Binding.oneWay (function _,{content = n} -> n)
    "RedirectToUser" |> Binding.cmd (fun (_,(model : ChatMessageInfo)) -> model.senderName |> UserName |> RedirectToUser)
]

let bindings () : Binding<Model,Msg> list = 
    [
        "Games" |> Binding.subModelSeq((function | {Model.GameList = games} -> games),(function | {LobbyId = idd} -> idd),serverInfoBindings )
        "ChatMessages" |> Binding.subModelSeq((function | {Model.ChatMsgs = msgs} -> msgs |> List.rev),(function | x -> x.senderName.GetHashCode() + x.content.GetHashCode()),chatMsgBindings)
        "MessageContent" |> Binding.twoWay((function | {ChatMsg = cmsg} -> cmsg),(fun newVal _ -> ChatMsgChanged newVal))
        "CreateLobby" |> Binding.cmd CreateLobby
        "SendMessage" |> Binding.cmd EmitChatMsg
        "LobbyWin" |> Binding.subModelWin(
            (fun (m) -> m.CreateLobbyWindow |> WindowState.ofOption),
            CreateLobbyMsg,
            (fun () -> [
                "CreateRequest" |> Binding.cmd LobbyCreator.CreateCommand
                "LobbyName" |> Binding.twoWay ((fun (_,{LobbyCreator.Model.LobbyName = LobbyCreator.LobbyName ln}) -> ln),(LobbyCreator.LobbyName >> LobbyCreator.NameChanged))
                "RoomSize" |> Binding.twoWay ((fun (_,{LobbyCreator.Model.LobbySize = sz}) -> sz.ToIndex()),(LobbyCreator.LobbySize.FromComboIndex >> Option.get >> LobbyCreator.SizeChanged))
                "Info" |> Binding.oneWay (function _,model -> model.GetStatusText())
                "CanClick" |> Binding.oneWay (function _,model -> model.CanClick())
            ]),
            (fun () -> LanucherView.WWindows.LobbyCreator(Owner = Application.Current.MainWindow))
        )
        "IsVisible" |> Binding.oneWay (fun x -> Visibility.Hidden)
        "UserMenuC" |> Binding.subModel(
            (fun _ -> UserMenu.Nothing),
            snd,
            UserMenuMsg,
            UserMenu.bindings)
    ]
