module Lobby

open Shared4.LauncherMsg
open Shared4.LobbyMsg
open System
open Akka.Actor
open Akka.FSharp
open Shared4.Common

let private connectionStrings = [
        ActorPath.Parse("akka.tcp://game-server-system@192.168.0.105:8090/user/game1")
        ActorPath.Parse("akka.tcp://game-server-system@192.168.0.105:8090/user/game2")
        ActorPath.Parse("akka.tcp://game-server-system@192.168.0.105:8090/user/game3")
    ]


type LobbyId = LobbyId of int

type LobbyUser = {
    info : (Guid * ActorRefWrapper<MsgToUi>)
    nick : string
    status : UserLobbyStatus
} with
    member this.ToInfo() = {
        nick = this.nick
        playerId = this.info |> fst
        status = this.status
    }

type ChatMsgContent = string
type PlayerName = string

type LobbyModel = {
    lobbyId : Guid
    name : string
    status : LobbyStatus
    size : int
    players : LobbyUser list
    msgs : (PlayerName * ChatMsgContent) list
    gameRoom : (ActorRefWrapper<Shared4.GameRoomMsg.ParentGameRoomMsg>*int) option
    actorResolver : ActorPath -> Async<IActorRef>
    nonActiveSince : DateTime option
}
    with 
        static member Init(name,creator : AddPlayer,mainActor : IActorRefFactory,?size : int) = 
            let fresh = {
                lobbyId = Guid.NewGuid()
                name = name
                status = Idle
                size = Option.defaultValue 10 size
                players = []
                msgs = []
                gameRoom = None
                actorResolver = fun (s : ActorPath) -> mainActor.ActorSelection(s).ResolveOne(System.TimeSpan.FromSeconds(5.0)) |> Async.AwaitTask
                nonActiveSince = None
            }

            fresh.AddPlayer creator

        member this.CreateInfo() = {
            Size = this.size
            PlayersCount = this.players.Length
            Status = this.status
            Name = this.name
            LobbyId = this.lobbyId
        }

        member this.AddPlayer (msg : AddPlayer) = 
            {
                this with
                    players = {info = (msg.guid,makeActorWrapper msg.reference);nick = msg.nick;status = UnReady} :: this.players
            }

let private lobbyUpdate (actor : Actor<obj>) (msg : LobbyMsg) (model : LobbyModel) =

    let playerInfo () = model.players |> List.find (function {info = (_,rf)} -> rf.actorRef = actor.Sender()) |> fun x -> x.ToInfo()

    let parent = { actorRef = actor.Context.Parent;dispatch = fun (m : Shared4.MenuServerMsg.FromLobby) -> actor.Context.Parent <! (Shared4.MenuServerMsg.MainMenuMsgs.FromLobby m)}

    let changePlayerStatusAndNotify msg = 
        let info = playerInfo ()

        let (msgToOthers,nextStatus) = match msg with | PlayerReady -> LobbyNotification.PlayerReady  , UserLobbyStatus.Ready | PlayerUnReady -> LobbyNotification.PlayerUnReady, UserLobbyStatus.UnReady | _ -> failwith "Pech"   

        let msg = {info with status = nextStatus} |> msgToOthers |> Notification.LobbyNotification |> MsgToUi.Notification

        model.players
        |> Seq.map (function | {info = (_,rf)} -> rf)
        |> Seq.iter (fun p -> p.dispatch msg)

        {model with players = model.players |> List.map (function | ({info = (_,rf)} as md) when rf.actorRef = actor.Sender() -> {md with status = nextStatus} | a -> a)}

    let sndOf3 (_,b,_) = b

    let (|StartingGame|_|) =
        function
        | PlayerReady when true || model.players |> Seq.filter (function | ({status = status}) -> status = Ready) |> Seq.length |> (<) 0 -> Some ()
        | _ -> None
   
    let handleSystemMsg = 
        function
        | (IsActive (MaxNonActiveDuration duration)) when model.nonActiveSince.IsNone || DateTime.Now - model.nonActiveSince.Value < duration  ->
              actor.Sender() <! true
              model
        | (IsActive _) ->
            actor.Sender() <! false
            {model with status = Dead}
        | GatherInfo when model.gameRoom.IsNone ->

            connectionStrings
            |> Seq.mapi (fun i connString ->
                async {
                    let! rf = model.actorResolver connString
                      
                    let! (resp : Choice<obj,exn>) = rf.Ask(Shared4.GameRoomMsg.AskForWeight,System.TimeSpan.FromSeconds(5.0)) |> Async.Catch

                    let r = match resp with
                            | Choice1Of2 r -> 
                                match (r :?> (Shared4.LobbyMsg.LobbyMsg)) with
                                | FromGameRoomParent (Weight m) -> Some (rf,m,i)
                                | _ -> None
                            | Choice2Of2 _ -> None
           
                    return r
                }
            )
            |> Async.Parallel
            |> fun cmp -> Async.StartWithContinuations(
                            cmp,
                            (Seq.choose id >> Seq.toList >> function | [] -> actor.Self <! (exn("No actor") |> CantFindGameRef |> FindRefResult |> SystemMsg ) | x -> x |> List.minBy sndOf3 |> (fun (a,_,c) -> (a,c)) |> FoundGameRef |> FindRefResult |> SystemMsg |> fun x -> actor.Self <! x),
                            (CantFindGameRef >> FindRefResult >> SystemMsg >> fun x -> actor.Self <! x),
                            ignore)

            model
        | FindRefResult (FoundGameRef (rf,index)) when model.gameRoom.IsNone ->
            let actorWrapper = {
                actorRef = rf
                dispatch = fun (x : Shared4.GameRoomMsg.ParentGameRoomMsg) -> rf <! x
            }

            rf <! (Shared4.GameRoomMsg.RoomRequest)

            {model with gameRoom = Some (actorWrapper,index)}
        | GameStartResult StartGame -> 
            failwith ""
        | FindRefResult (CantFindGameRef ex) ->
            printfn "Problem with finding gameRef %A " ex
            failwith "Failing :)"
    
    let handleFromGameRoomParent = 
        function
        | (Weight _) ->
            failwith ""
        | (LobbyRefStr refStr) ->

            let gameRoomServerNumber = model.gameRoom.Value |> snd |> Shared4.LauncherMsg.GameRoomNumber.FromInt

            let startGameMsg2 = { token = refStr; which = gameRoomServerNumber } |> Shared4.LauncherMsg.InitGameActor |> Shared4.LauncherMsg.SystemMsg

            model.players 
            |> Seq.map ((fun x -> x.info) >> snd >> fun x -> x.actorRef) 
            |> Seq.iter (fun x -> x <! startGameMsg2)

            model

    let handleFromUser = 
        function
        | PlayerLeft | PlayerConnectionLost as x ->
            
            let sender = actor.Sender()

            let msg = 
                model.players
                |> List.find (function {info = (_,rf)} -> rf.actorRef = sender)
                |> fun x -> x.ToInfo()
                |> UserLeft
                |> LobbyNotification
                |> Notification
            
            let newPlayersList = model.players |> List.filter (function {info = (_,rf)} -> rf.actorRef <> sender)

            newPlayersList
            |> Seq.map (function | {info = (_,rf)} -> rf)
            |> Seq.iter (fun p -> p.dispatch msg)

            let newModel = {model with players = newPlayersList}

            if x = PlayerLeft then
                 sender 
                 |> Shared4.MenuServerMsg.UserRef 
                 |> Shared4.MenuServerMsg.RejoinMenu
                 |> parent.dispatch
               
            parent.dispatch (Shared4.MenuServerMsg.LobbyChanged <| newModel.CreateInfo())

            let newActive = if newPlayersList.Length = 0 then Some DateTime.Now else None

            {newModel with nonActiveSince = newActive}
        | StartingGame ->
            model.players
            |> Seq.map (function | {info = (_,rf)} -> rf)
            |> Seq.iter (fun p -> p.dispatch (GameStarting |> LobbyNotification |> Notification))
            
            let gameActor = model.gameRoom.Value |> fst
            
            askActor<LobbyMsg,LobbyMsg> 
                actor.Self 
                gameActor.actorRef
                id
                (CantStartGame >> GameStartResult >> SystemMsg)
                Shared4.GameRoomMsg.ParentGameRoomMsg.StartGame

            let newModel = changePlayerStatusAndNotify PlayerReady

            {newModel with status = Starting}
        | (PlayerReady | PlayerUnReady) as msg ->
           
           let newModel = changePlayerStatusAndNotify msg

           {newModel with status = newModel.status}
   
        | PlayerChatMsg msg ->

            let info = playerInfo()
            let msg = (info , msg) |> NewUserChatMsg |> LobbyNotification |> Notification

            let sender = actor.Sender()

            let confirmation = ChatMsgDelivered |> Confirmation

            ActorUtil.sendToUser sender confirmation |> ignore

            model.players
            |> Seq.map (function | {info = (_,rf)} -> rf)
            |> Seq.filter (fun rf -> rf.actorRef <> sender)
            |> Seq.iter (fun p -> p.dispatch msg)

            model

    match msg with
    | SystemMsg systemMsg ->

        systemMsg |> handleSystemMsg

    | FromGameRoomParent m -> m |> handleFromGameRoomParent    
    
    | AddPlayer ({reference = rf} as msg) ->
        
        actor.WatchWith(rf,PlayerConnectionLost)
        |> ignore

        let model = model.AddPlayer msg

        let info = model.players.Head.ToInfo() |> NewUserJoined |> Notification.LobbyNotification |> Notification
        
        let self = actor.Self

        let msgs = 
            model.msgs
            |> List.map (fun (pName,content) -> 
                {senderName = pName;content = content}
            )

        let lobbyInfo = JoinedLobby {
            users = model.players.Tail |> List.map (fun v -> v.ToInfo())
            lobbyRef = Akka.Serialization.Serialization.SerializedActorPath self
            msgs = msgs
        } 

        rf <! (lobbyInfo |> Confirmation |> MsgToUi)

        parent.dispatch (Shared4.MenuServerMsg.LobbyChanged <| model.CreateInfo()) 

        model.players.Tail
        |> Seq.map (function | {info = (_,rf)} -> rf)
        |> Seq.iter (fun p -> p.dispatch info)

        {model with nonActiveSince = None}

    | FromUser m -> m |> handleFromUser
     
let onOther (_ : Actor<obj>) (_ : obj) (model : LobbyModel) = 
    model
 
let createLobbyActor mainActor name size creator =

    let initialModel = LobbyModel.Init(name,creator,mainActor,size)
    let actRef = ActorFactory.createActor mainActor None initialModel lobbyUpdate onOther

    let wrapper = {actorRef = actRef;dispatch = fun (m : LobbyMsg) -> actRef <! m}

    GatherInfo
    |> SystemMsg
    |> wrapper.dispatch

    initialModel.CreateInfo(),wrapper
    
            
