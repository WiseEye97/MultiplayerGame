module ActorFactory

open Akka.Actor
open Akka.FSharp
open Newtonsoft.Json

module Factory =

    type Msg<'T> = 
        | Normal of 'T
        | Other of obj
    
    let emptyOther = fun _a _b c -> c 
    
    let createActor<'T,'M> (parent : IActorRefFactory) name initial update onOther =
    
        let name = Option.defaultValue (System.Guid.NewGuid().ToString()) name
    
        spawn parent name (fun (mailbox) ->
            let rec loop (model : 'M) = actor {
                let! (message : obj) = mailbox.Receive()
    
                let newModel =
                    match message with
                    | :? 'T as x -> update mailbox x model
                    | msg ->
                        let stringify = message.ToString()
                        let fromJson =
                            try
                                JsonConvert.DeserializeObject<'T>(stringify)
                                |> Some
                            with
                                | ex -> 
                                    None
    
                        fromJson 
                        |> function 
                            | Some x -> update mailbox x model 
                            | _ -> onOther mailbox msg model
                        
    
                // handle an incoming message
                return! loop newModel
            }
            loop <| initial)

