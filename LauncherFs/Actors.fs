module Actors

open Akka.Actor
open Akka.FSharp
open Common
open Elmish
open Newtonsoft.Json
open Hyperion
open System

let randomPort = Random().Next(8050,8075)

let configString = sprintf """
            akka {
                actor {
                    provider = remote
                    serializers {
                        json = "Serializer+LauncherSerializer, Shared4"
                    }
                    serialization-bindings {
                        "System.Object" = json
                    }
                }
       
                remote {
                    dot-netty.tcp {
                        port = %d
                        hostname = localhost
                    }
                }
            }
       """ 
let configString2 = configString randomPort

type Msg<'T> = 
    | Normal of 'T
    | Other of obj

let emptyOther = fun _a _b c -> c 

let createWPFActor<'T,'M> (parent : IActorRefFactory) name initial update onOther (dispatch : Dispatch<WPFMsg>) =

    let name = Option.defaultValue (System.Guid.NewGuid().ToString()) name

    spawn parent name (fun (mailbox) ->
        let rec loop (model : 'M) = actor {
            let! (message : obj) = mailbox.Receive()

            let newModel =
                match message with
                | :? 'T as x -> update mailbox x model dispatch
                | msg -> 
                    let stringify = message.ToString()
                    let fromJson =
                        try
                            JsonConvert.DeserializeObject<'T>(stringify)
                            |> Some
                        with
                            | _ -> None

                    fromJson 
                    |> function 
                        | Some x -> update mailbox x model dispatch
                        | _ -> onOther mailbox msg model

            // handle an incoming message
            return! loop newModel
        }
        loop <| initial mailbox.Self)
