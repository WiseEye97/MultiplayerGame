module MainActor

open Shared4.MenuServerMsg
open Shared4.LauncherMsg
open Shared4.Common
open Akka.Actor
open Akka.FSharp
open System
open Shared4.LobbyMsg
open Shared4.HttpTypes

type MainMenuModel = {
    games : (ServerGameInfo*ActorRefWrapper<LobbyMsg>) list
    idDictionary : Map<IActorRef,Guid>
    users : (MenuUser*ActorRefWrapper<MsgToUi>) list
    globalChatMessagess : ChatMessageInfo list
}
    with 
        static member Init() = {globalChatMessagess = [];idDictionary = Map.empty;users = [];games = []}

        member this.ConstructInfo() = this.games |> List.map fst |> fun li -> {ServerList = li;ChatMsgs = this.globalChatMessagess}
            

let update (mailbox : Actor<obj>) msg model =

    let sender = mailbox.Sender()

    let sendToPlayer = PlayerController.sendMsgToPlayer sender

    let playerModel = fun () -> model.users |> List.find (snd >> (fun x -> x.actorRef) >> ((=) sender)) |> fst

    let handleHttp =
        function
        | UserRegistration (UserRegistration.Registered,sender) ->
            sender <! (Shared4.LauncherMsg.Registered |> Confirmation |> MsgToUi)
            model
        | UserRegistration (UserRegistration.NotRegistered s,sender) ->
            sender <! (Shared4.LauncherMsg.NotRegistered |> Confirmation |> MsgToUi)
            model
        | UserRegistration (UserRegistration.ErrWhileRegistering _,sender) ->
            sender <! (Shared4.LauncherMsg.RegisteredServerError |> Confirmation |> MsgToUi)
            model
        | UserAuth (RegisterAuthenticated r) ->
            let menuUser = {
                       nick = r.Info.nickName
                       status = Registered
                   }

            let playerWrapper = {actorRef = r.SenderRef;dispatch = (fun (m : MsgToUi) -> r.SenderRef <! (MsgToUi m))}

            model.games 
            |> List.map fst
            |> fun li -> {ServerList = li;ChatMsgs = model.globalChatMessagess}
            |> (Logged >> Confirmation)
            |> playerWrapper.dispatch

            {model with idDictionary = Map.add r.SenderRef (Guid.NewGuid()) model.idDictionary; users = (menuUser,playerWrapper) :: model.users}
        | UserAuth (WrongCred r) ->
            let playerWrapper = {actorRef = r.SenderRef;dispatch = (fun (m : MsgToUi) -> r.SenderRef <! (MsgToUi m))}

            playerWrapper.dispatch (Confirmation <| WrongCreditenials)

            model
        | UserAuth (CannotAuthenticate r) ->
            let p = {actorRef = r.SenderRef;dispatch = (fun (m : MsgToUi) -> r.SenderRef <! (MsgToUi m))}
            //TODO Log error Somewhere

            ServerProblem |> Confirmation |> p.dispatch

            model
    
    let handleSystem = 
        function
        | ErrorWhileCleaning _err ->
            //Report Error somewhere xD
            model
        | ToDispose li ->
            for l in li do l.actorRef.Tell(PoisonPill.Instance)

            let deadRefs = li |> Seq.map (fun x -> x.actorRef) |> Set.ofSeq

            let (newGames,toDelete) =  model.games |> List.partition (fun (_,wrapper) -> deadRefs.Contains wrapper.actorRef |> not)

            let notif = toDelete |> List.map fst |> DeleteServers |> Notification

            model.users
            |> Seq.choose (function ({status = Registered},wrapper) -> Some wrapper.dispatch | _ -> None)
            |> Seq.iter ((|>) notif)

            {model with games = newGames}
        | CleanEmpty -> 
            model.games
            |> Seq.choose (fun (info,wrapper) -> if info.Status = Idle then Some wrapper else None)
            |> Seq.map (fun wrapper ->
                let msgToSend = TimeSpan.FromMinutes(2.0) |> MaxNonActiveDuration |> IsActive |> SystemMsg
                async {
                    let! (isActive : Choice<obj,exn>) = wrapper.actorRef.Ask(msgToSend,TimeSpan.FromSeconds(5.0)) |> Async.Catch
                    let r = match isActive with
                            | Choice1Of2 x -> 
                                match x with
                                | :? bool as isActive -> isActive
                                | _ -> false
                            | _ -> false
                    return (wrapper,r)
                }
            )
            |> Async.Parallel
            |> fun cmp -> 
                Async.StartWithContinuations(
                    cmp,
                    (Seq.filter (not << snd) >> Seq.map fst >> Seq.toList >> ToDispose >> SystemMenuMsg >> fun x -> mailbox.Self <! x),
                    (fun e -> mailbox.Self <! (e |> ErrorWhileCleaning |> SystemMenuMsg)),
                    ignore
                )

            model
    
    let handleFromUser = 
        function
        | CreateNewUser req ->
            HttpUtil.createUser
                    req
                    (function
                        | RegisteredOk -> mailbox.Self <!  ((UserRegistration.Registered,sender) |> UserRegistration |> FromHttp)
                        | Klops s -> mailbox.Self <! ((UserRegistration.NotRegistered s,sender) |> UserRegistration |> FromHttp))
                    (fun err -> mailbox.Self <! ((UserRegistration.ErrWhileRegistering err,sender) |> UserRegistration |> FromHttp))
            |> Async.Start

            model
        | MenuChatMsg msg ->
               let senderModel = playerModel ()
               
               let msgToSend = GlobalChatMsg {senderName = senderModel.nick;content = msg} |> Notification

               model.users
               |> Seq.choose (function | ({status = status},rf) when status = UserStatus.Registered && rf.actorRef <> sender -> Some rf | _ -> None)
               |> Seq.iter (fun rf -> rf.dispatch msgToSend)

               let newChatMsg = {
                   content = msg
                   senderName = senderModel.nick
               }

               ChatMsgDelivered
               |> (Confirmation >> MsgToUi)
               |> sendToPlayer

               {model with globalChatMessagess = newChatMsg :: model.globalChatMessagess}
        | RegisterUser user -> 
               HttpUtil.authenticate
                       user
                       (function
                           | Success {Token = token} -> mailbox.Self <! (UserAuth.CreateFromSuccess(token,user,sender) |> UserAuth |> FromHttp)
                           | Failure -> mailbox.Self <! ( UserAuth.CreateFromKlops(sender) |> UserAuth |> FromHttp))
                       (fun err -> mailbox.Self <! (UserAuth.CreateFromFailure(err,sender) |> UserAuth |> FromHttp))
               |> Async.Start

               model
        | CreateLobby {serverName = name;roomSize = size} ->
               
               let playerId = model.idDictionary.[sender]

               let playerModel = playerModel()

               let thisActor = mailbox.Context
               let lobbySize = size

               let lobbyPair = Lobby.createLobbyActor thisActor name lobbySize  {guid = playerId ; reference = sender;nick = playerModel.nick}

               lobbyPair
               |> snd
               |> fun x -> x.actorRef
               |> Akka.Serialization.Serialization.SerializedActorPath
               |> MyLobbyInfo.Create
               |> (LobbyCreated >> Confirmation >> MsgToUi)
               |> sendToPlayer 

               let info = lobbyPair |> fst |> NewLobby |> Notification

               let newUsers = 
                   model.users 
                   |> List.map (function | ({status = Registered} as stat,act) when act.actorRef = sender -> ({stat with status = InLobby},act) | x -> x)

               newUsers
               |> Seq.choose (function | ({status = Registered},act) -> Some act | _ -> None)
               |> Seq.iter (fun rf -> rf.dispatch info)
                
               {model with games = lobbyPair :: model.games;users = newUsers}

        | JoinServer lobbyId ->
            let lobby = 
                model.games
                |> Seq.pick (function | ({LobbyId = lid} ,rf) when lid = lobbyId -> Some rf | _ -> None)
               
            let userName = (playerModel ()).nick

            let playerId = model.idDictionary.[sender]

            let msg = AddPlayer {guid = playerId ; reference = sender;nick = userName}

            lobby.dispatch msg

            let newUsers = model.users |> List.map (fun (usr,x) -> if usr.nick = userName then ({usr with status = UserStatus.InLobby},x) else (usr,x))

            {model with users = newUsers}
       
    let handleFromLobby = 
        function
        | LobbyChanged lobbyInfo ->
            let newGames =
                model.games
                |> List.map (function | (_,rf) when sender = rf.actorRef -> (lobbyInfo,rf) | x -> x)

            model.users
            |> Seq.choose (function | ({status = status},rf) when status = UserStatus.Registered -> Some rf  | _ -> None)
            |> Seq.iter (fun rf -> rf.dispatch (lobbyInfo |> ServerChanged |> Notification))

            {model with games = newGames}
        | RejoinMenu (UserRef ref) ->
            let wrapper = model.users |> List.find (snd >> (fun x -> x.actorRef = ref)) 
            let msgToSend = model.ConstructInfo() |> BackToMenu |> Confirmation
            (snd wrapper).dispatch msgToSend
            {model with users = model.users |> List.map (fun (a,b) -> ({a with status = UserStatus.Registered},b))}

    match msg with
    | FromHttp http -> http |> handleHttp
    | SystemMenuMsg systemMsg -> systemMsg |> handleSystem
    | MainMenuMsgs.FromUser fromUserMsg -> fromUserMsg |> handleFromUser
    | FromLobby lobbyMsg -> lobbyMsg |> handleFromLobby
    
let myActor system = 
    let res = ActorFactory.createActor<MainMenuMsgs,MainMenuModel> 
                system 
                (Some "test1") 
                (MainMenuModel.Init()) 
                update 
                (fun _mailbox (_msg : obj) (model: MainMenuModel) ->
                    let x = 1
                    model
                )

    let timer = new System.Timers.Timer(50000.0)
    timer.AutoReset <- true
    timer.Elapsed.Add(fun _x -> CleanEmpty |> SystemMenuMsg |> res.Tell)
    timer.Start()

    res



