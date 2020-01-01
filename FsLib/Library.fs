namespace FsLib

open Godot
open Akka.FSharp
open System.Collections.Concurrent

open PlayerController
open System
open Akka.Actor
open SharedTypes.GameTypes
open Akka.Remote

type Entity = {
    body : KinematicBody
    mutable velocity : Vector3
}

type RootFs() as this =
    inherit Spatial()

    let multiplayerEvents = ConcurrentQueue<GameCommands>()
    let otherPlayers = System.Collections.Generic.List<Entity>()

    let anim = lazy(
        this.GetNode(new NodePath("Player"))
        :?> KinematicBody
    )
    let velocity = ref <| Vector3(0.0f,0.0f,0.0f)
    let x : option<RemoteActorRef> = None;
    let system = System.create "godot-system" (Configuration.parse(Config.configString))

    let actorSystem : ExtendedActorSystem = downcast system  
    
    let ref = 
        ActorFactory.Factory.createActor 
            system 
            (Some "gameactor") 
            (Actor.init actorSystem) 
            (Actor.update (fun t -> multiplayerEvents.Enqueue(t)))
            ActorFactory.Factory.emptyOther

    let dispatch = fun (x : SharedTypes.GameServerTypes.GameNotification) -> ref <! (x |> SharedTypes.GameTypes.ToServer)

    override __._Ready() =
           GD.Print("Hello from F#!")
    
    member private this.HandleServerTask task = 
        match task with
        | NoTask -> ()
        | SetMyPosition (x,y) ->
            printfn "Set My Pos At %A" (x,y)
            anim.Value.Translation <- Vector3(x,0.0f,y)
        | SpawnOpponent opInfo ->
            
            printfn "Spawn Opponent Info -> %A" opInfo

            let duplicated = anim.Value.Duplicate() :?> KinematicBody
            duplicated.SetName(opInfo.name)
            
            let (x,y) = opInfo.pos
            duplicated.Translation <- Vector3(x,0.0f,y)

            otherPlayers.Add({body = duplicated;velocity = Vector.zero})

            this.AddChild duplicated
        | OpponentMove (info,ev) ->
            printfn "Opponent Moving -> %A" (info,ev)
            let entity = otherPlayers.Find(fun en -> en.body.Name = info.name)

            let newVector = Vector.applyEvent entity.velocity ev
            entity.velocity <- newVector


    override this._Process(_delta) =
        let mutable task = NoTask

        let _ = multiplayerEvents.TryDequeue(&task)

        try
            task |> this.HandleServerTask
        with
        | ex ->
            ()

        let ev = PlayerController.Vector.genEvent()

        if ev <> SharedTypes.Common.SlowDown then
            printfn "Sending Event -> %A" ev

            ev 
            |> SharedTypes.GameServerTypes.MoveEvent 
            |> dispatch 
            
        let newVector = Vector.applyEvent velocity.Value ev

        velocity := newVector

        otherPlayers.ForEach(fun ent ->
            ent.velocity <- Vector.applyEvent ent.velocity SharedTypes.Common.SlowDown
        )

        anim.Value.MoveAndSlide(newVector) |> ignore
