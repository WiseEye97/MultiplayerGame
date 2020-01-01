module Serializer

open Newtonsoft.Json
open System.Text
open Akka.Serialization
open System
open SharedTypes

type MySerializer(system) =
    inherit Serializer(system)

    let addTypeHint (tp : Type) binary =
        let tpHintBytes = 
            tp.FullName.Split('+').[1]
            |> Encoding.UTF8.GetBytes
        let tpHintArr = Array.init 40 (fun i -> if tpHintBytes.Length > i then tpHintBytes.[i] else byte 0)
        Array.concat [tpHintArr;binary]

    override __.IncludeManifest = false

    override __.Identifier = 1234567

    override __.ToBinary obj =
        let tp = obj.GetType()

        obj
        |> JsonConvert.SerializeObject 
        |> Encoding.UTF8.GetBytes
        |> addTypeHint tp

    override __.FromBinary(_,_) =
        obj()

let fromBytes<'T> bytes = 
    let str = Encoding.UTF8.GetString bytes
    let res : obj = upcast JsonConvert.DeserializeObject<'T>(str)
    res

type GameSerializer(system) = 
    inherit MySerializer(system)

    let makeTypeStr (tp : Type) = tp.FullName.Split('+').[1]

    let tp1 = makeTypeStr typeof<GameTypes.Msg>
    let tp2 = makeTypeStr typeof<GameServerTypes.GameNotification> 
    
    override __.FromBinary(bytes,_ttype) =
        let typeInfo = bytes.[0..39] |> Array.takeWhile ((<>) (byte 0)) |> Encoding.UTF8.GetString
        
        bytes.[40..]
        |>
        (if typeInfo = tp1 then 
            fromBytes<GameTypes.Msg>
        elif typeInfo = tp2 then
            fromBytes<GameServerTypes.GameNotification>
        else 
            failwith "")