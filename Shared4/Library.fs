namespace Shared4

open Akka.Actor
open Akka.FSharp
open System

module HttpTypes = 
    
    type TokenResult = {
        Token : string
    }

    type AuthResult = 
        | Success of TokenResult
        | Failure

    type RegistrationResult = 
        | RegisteredOk
        | Klops of string

    type RegistrationRequest = {
        Nick : string
        Mail : string
        Password : string
    }
 
    type User = {
        Name : string
        DateOfRegistration : DateTime
        PasswordHash : string 
    }

module Common = 
    
    type PlayerProfileInfo = {
        nick : string
        joinDate : DateTime
        isFriend : bool
    }

    type LobbyStatus = 
        | Playing
        | Idle
        | Starting
        | Dead

    type ServerGameInfo = {
        Size : int
        PlayersCount : int
        Status : LobbyStatus
        Name : string
        LobbyId : Guid
    }

    type UserStatus = 
        | Registered
        | InLobby

    type MenuUser = {
        nick : string
        status : UserStatus
    }
    
    type GlobalChatMsg = {
        Content : string
        Author : MenuUser
    }

    type ActorRefWrapper<'M> = {
        actorRef : IActorRef
        dispatch : 'M -> unit
    }

    let askActor<'m,'r> (sender : IActorRef) (reciever : IActorRef) onSuccess onError = 
        fun m ->
            let dispatch = fun (x : 'm) -> sender <! x
            let cmp = reciever.Ask(m,TimeSpan.FromSeconds(3.0))
            Async.StartWithContinuations(
                cmp,
                ((fun (x : obj) -> x :?> 'r) >> onSuccess >> dispatch),
                (onError >> dispatch),
                ignore
            )

    let asyncCmd<'M,'R> (dispatch : 'M -> unit) (computation : Async<'R>) onSuccess onError =
        Async.StartWithContinuations(
                       computation,
                       (onSuccess >> dispatch),
                       (onError >> dispatch),
                       ignore
                   )

module Models = 
    type Player = {
        nick : string

    } 

module LobbyMsg = 
    
    type AddPlayer = {
        guid : Guid
        reference : IActorRef
        nick : string
    }

    type MaxNonActiveDuration = MaxNonActiveDuration of TimeSpan

    type FindRefResult = 
        | FoundGameRef of IActorRef*int
        | CantFindGameRef of exn

    type GameStartResult = 
        | CantStartGame of exn
        | StartGame

    type SystemMsg = 
        | GatherInfo
        | FindRefResult of FindRefResult
        | GameStartResult of GameStartResult
        | NotifyAboutLobby of string
        | IsActive of MaxNonActiveDuration
        | GameEnding
        | GameEnded
        
    type FromGameRoomParent = 
        | Weight of int
        | LobbyRefStr of string
    
    type FromUser = 
        | PlayerLeft
        | PlayerReady
        | PlayerUnReady
        | PlayerChatMsg of string
        | PlayerConnectionLost 

    type LobbyMsg =
        | AddPlayer of AddPlayer
        | FromUser of FromUser
        | SystemMsg of SystemMsg
        | FromGameRoomParent of FromGameRoomParent

    let makeLobbyRefStr = LobbyRefStr >> FromGameRoomParent

    let sendMsg (actor : IActorRef) (msg : LobbyMsg) = actor <! msg

module MenuServerMsg =

    type Registered = {
        nickName : string
        passWord : string
    }
    
    type CreateLobby = {
        serverName : string
        roomSize : int
    }

    type UserRef = UserRef of IActorRef

    type UserAuth = 
        | RegisterAuthenticated of {|TokenRes : string;Info : Registered;SenderRef : IActorRef|}
        | CannotAuthenticate of {|Reason : exn;SenderRef : IActorRef|}
        | WrongCred of {|SenderRef : IActorRef|}
        with 
            static member CreateFromSuccess(tokenRes,info ,senderRef) = RegisterAuthenticated {|TokenRes = tokenRes ;Info = info;SenderRef = senderRef|}
            static member CreateFromKlops(senderRef) = WrongCred {|SenderRef = senderRef|}
            static member CreateFromFailure(error,senderRef) = CannotAuthenticate {|Reason = error;SenderRef = senderRef|}
    
    type UserRegistration = 
        | Registered
        | NotRegistered of string
        | ErrWhileRegistering of exn

    type SystemMenuMsg = 
          | CleanEmpty
          | ToDispose of Common.ActorRefWrapper<LobbyMsg.LobbyMsg> list
          | ErrorWhileCleaning of exn

    type FromUser = 
        | CreateNewUser of HttpTypes.RegistrationRequest
        | RegisterUser of Registered
        | CreateLobby of CreateLobby
        | MenuChatMsg of string
        | JoinServer of Guid
      
    type FromLobby =
        | LobbyChanged of Common.ServerGameInfo
        | RejoinMenu of UserRef

    type FromHttp = 
        | UserAuth of UserAuth
        | UserRegistration of UserRegistration* sender : IActorRef

    type MainMenuMsgs = 
        | FromUser of FromUser
        | FromLobby of FromLobby
        | FromHttp of FromHttp
        | SystemMenuMsg of SystemMenuMsg

    let sendMsg (actor : IActorRef) (msg : MainMenuMsgs) = actor <! msg
 
module GameRoomMsg = 
    
    type ParentGameRoomMsg = 
        | AskForWeight
        | StartGame
        | RoomRequest
        | JoinPlayer of string

module LauncherMsg = 
    
    type ChatMessageInfo = {
        senderName : string
        content : string
    }

    type LoggedInfo = {
        ServerList : Common.ServerGameInfo list
        ChatMsgs : ChatMessageInfo list
    }

    type UserLobbyStatus = 
        | UnReady
        | Ready
        | InGame
        with
            member this.ToStr() = 
                match this with
                | UnReady -> "NIEGOTOWY"
                | Ready -> "GOTOWY"
                | InGame -> "W grze..."

    type ServerStatus = 
        | InGame
        | InActive

    type LobbyUserInfo = {
        nick : string
        playerId : Guid
        status : UserLobbyStatus
    }

    type UserJoinedLobby = {
        nickName : string
    }

    type JoinedLobbyInfo = {
        users : LobbyUserInfo list
        msgs : ChatMessageInfo list
        lobbyRef : string
    }

    type GameRoomNumber = 
        | First
        | Second
        | Third

        with
            static member FromInt = 
                function
                | 0 -> First
                | 1 -> Second
                | 2 -> Third
                | _ -> failwith "Wrong index"

            member this.ToInt() = 
                match this with 
                | First -> 0
                | Second -> 1
                | Third -> 2

    type StartGame = {
        token : string
        which : GameRoomNumber
    }

    type StartGameRef = {
        token : string
        gameActor : unit -> Async<IActorRef>
        which : GameRoomNumber
    }

    type SystemMsg = 
        | SendAck
        | SendToServer of MenuServerMsg.MainMenuMsgs
        | AddLobbyActor of IActorRef
        | SendToLobby of LobbyMsg.FromUser
        | InitGameActor of StartGame

   
    type LobbyNotification = 
        | NewUserJoined of LobbyUserInfo
        | PlayerReady of LobbyUserInfo
        | PlayerUnReady of LobbyUserInfo
        | UserLeft of LobbyUserInfo
        | NewUserChatMsg of LobbyUserInfo*string
        | GameStarting
        | StartGameRef of StartGameRef

    type Notification = 
        | NewLobby of Common.ServerGameInfo
        | LobbyNotification of LobbyNotification 
        | ServerChanged of Common.ServerGameInfo
        | GlobalChatMsg of ChatMessageInfo
        | DeleteServers of Common.ServerGameInfo list
        

    type MyLobbyInfo = MyLobbyInfo of JoinedLobbyInfo
        with
            static member Create actorReference = MyLobbyInfo {
                users = []
                msgs = []
                lobbyRef = actorReference
            }

    type Confirmation = 
        | Logged of LoggedInfo
        | WrongCreditenials
        | ServerProblem
        | JoinedLobby of JoinedLobbyInfo
        | LobbyCreated of MyLobbyInfo
        | ChatMsgDelivered
        | BackToMenu of LoggedInfo
        | Registered
        | NotRegistered
        | RegisteredServerError

    type MsgToUi = 
        | Confirmation of Confirmation
        | Notification of Notification

    type LauncherMsgs = 
        | MsgToUi of MsgToUi
        | SystemMsg of SystemMsg

    let sendMsg (actor : IActorRef) (msg : MsgToUi) = actor <! (MsgToUi msg)

    let makeActorWrapper (actor : IActorRef) = {
        Common.ActorRefWrapper.actorRef = actor
        Common.ActorRefWrapper.dispatch = fun (m : MsgToUi) -> actor <! (MsgToUi m)
    }
        

module Messages =
    
    type JoinPlayerToServer = {
        serverId : int
        playerId : int
    }

    type NewPos = {
        pos_x : int
        pos_y : int
    }

    type MsgFromPlayer = 
        | PlayerInfo of JoinPlayerToServer
        | ChangePosition of NewPos
        | PlayerShoot

    type PlayerJoined = {
        tcpConnection : IActorRef
        playerActor : IActorRef
        info : JoinPlayerToServer
    }

    type GameRoomMsgs = 
           | AddPlayer of JoinPlayerToServer
           | MovePlayer of NewPos
           | StartGame
           | PlayerShootEvent

    type SystemMessage = {
        Content : string
    }
    
    type UserMessage = {
        From : int
        Content : string
    }
    

    type ViewUserMessage = {
        From : string
        Content : string
    }

    type ViewMessage = 
        | SystemMessage of SystemMessage
        | UserMessage of ViewUserMessage

    type Message = 
           | SystemMessage of SystemMessage
           | UserMessage of UserMessage
       
    type CreateGameRequest = {
        gameId : int
        size : int
    }

    type MessagesToServer = 
        | CreateGame of CreateGameRequest
        | NewPlayer of JoinPlayerToServer

    type GameServerMsg = 
        | GameEnded
            
    type InRoomStatus = 
        | Ready
        | UnReady
        | Playing
    
    type PlayerInfo = {
        Name : string
        Status : InRoomStatus
    }

    type NewChatMessage = { 
        ChatMessage : string
    }

    type MsgInfo = {
        Content : string
        From : string
    }

    type CreateRoom = {
        ServerName : string
    }

    type JoinServer = {
        ServerID : int
    }

    type Register = {
        Nick : string
        Password : string
    }

    type ServerGameInfo = {
        Size : int
        PlayersCount : int
        Status : string
        GameId : int
        Name : string
    }

    type GamesList = {
        Games : ServerGameInfo list
    }

    type JoinedOk = {
        Players : PlayerInfo list
        Messages: ViewMessage list
    }

    type NewUser = {
         Name : string
         Status : obj
    }

    type Registered = {
        Success : bool
    }

    type JoinServerResponses = 
        | GameFull
        | JoinedOk of JoinedOk
        | NoGameFound
       
    let makeJoinResp = JoinedOk
    let makeNGF = NoGameFound
    let makeGF = GameFull

    type Requests = 
        | CreateRoom of CreateRoom
        | GetGames
        | JoinServer of JoinServer
        | OnChatMessage of NewChatMessage
        | OnLeave
        | OnReady
        | UnReady
        | Register of Register
        | ServerCreated of int

    type Responses = 
        | JoinServerResponses of JoinServerResponses
        | GamesList of GamesList
        | NewUser of NewUser
        | Registered of Registered
        | ServerChanged of ServerGameInfo
        | UserLeft of PlayerInfo
        | UserReady of PlayerInfo
        | UserUnReady of PlayerInfo
        | NewServer of ServerGameInfo
        | NewMsg of MsgInfo
        | GameStarting of JoinPlayerToServer

    type SystemMsgs = 
        | UserDisconected

    let usD = UserDisconected

    type TcpMsgs = 
        | Req of Requests
        | Res of Responses
        | MsgToGameServer of MessagesToServer
        | SystemMsgs of SystemMsgs
        | GameServerMsg of GameServerMsg

    


   
   
