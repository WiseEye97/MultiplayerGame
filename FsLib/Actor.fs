module Actor

open Akka.Actor
open Akka.FSharp
open SharedTypes.GameTypes
open SharedTypes.GameServerTypes

let private connectionStrings = [
    ActorPath.Parse("akka.tcp://game-server-system@192.168.0.105:8090/user/game1")
    ActorPath.Parse("akka.tcp://game-server-system@192.168.0.105:8090/user/game2")
    ActorPath.Parse("akka.tcp://game-server-system@192.168.0.105:8090/user/game3")
]

type Inited = {
    serverDispatch : SharedTypes.GameServerTypes.GameNotification -> unit
}

type Model = 
    | WaitingForInit of actorResolver : (ActorPath -> Async<IActorRef>)* actorLobbyResolver : (string -> IActorRef)
    | Inited of Inited

let private dispatchToServer ref (x : SharedTypes.GameServerTypes.GameNotification) = ref <! x

let init (actorSys : ExtendedActorSystem) = 
    WaitingForInit ((fun path -> actorSys.ActorSelection(path).ResolveOne(System.TimeSpan.FromSeconds(5.0)) |> Async.AwaitTask),fun s -> actorSys.Provider.ResolveActorRef(s))
    

let update dispatch (actor : Actor<obj>) (msg : Msg) model = 
    match msg,model with
    | LobbyResolved (lobbyStr,myName),WaitingForInit (_,resolver2) ->
        printfn "LobbyStr -> %A" lobbyStr

        let lobbyRef = resolver2 lobbyStr
        let newModel = {serverDispatch = dispatchToServer lobbyRef}

        newModel.serverDispatch (RegisterPlayer myName)

        Inited newModel
    | InitialFromLauncher initial,WaitingForInit (resolver,resolver2) ->
        printfn "I got initial Msg !"

        let serverPath = connectionStrings.[initial.serverIndex]

        let registerMsg = {
            name = initial.myName
            token = initial.lobbyToken
        }

        let cmp = async {
            let! rf = resolver serverPath
            rf.Tell(registerMsg,actor.Self) 
        }

        Async.Start cmp

        model
    | InitialFromLauncher _ , _ -> failwith "klops"
    | GameCommands com,Inited _ -> dispatch <| com;model
    | ToServer m,Inited i -> i.serverDispatch m;model
    | _ -> sprintf "Wrong Msg %A" msg |> failwith
        