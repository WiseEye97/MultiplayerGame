module Serializer

open Newtonsoft.Json
open System.Text
open Newtonsoft.Json.Linq
open System.Net.Sockets
open Akka.Serialization
open System
open Shared4.MenuServerMsg
open SharedTypes

type MySerializer(system) =
    inherit Serializer(system)

    let addTypeHint (tp : Type) binary =
        let tpHintBytes = 
            tp.FullName.Split('+').[1]
            |> Encoding.UTF8.GetBytes
        let tpHintArr = Array.init 40 (fun i -> if tpHintBytes.Length > i then tpHintBytes.[i] else byte 0)
        Array.concat [tpHintArr;binary]

    override this.IncludeManifest = false

    override this.Identifier = 1234567

    override this.ToBinary obj =
        let tp = obj.GetType()

        obj
        |> JsonConvert.SerializeObject 
        |> Encoding.UTF8.GetBytes
        |> addTypeHint tp

    override this.FromBinary(bytes,ttype) =
        obj()

let fromBytes<'T> bytes = 
    let str = Encoding.UTF8.GetString bytes
    let res : obj = upcast JsonConvert.DeserializeObject<'T>(str)
    res

type MenuSerializer(system) = 
    inherit MySerializer(system)

    let makeTypeStr (tp : Type) = tp.FullName.Split('+').[1]

    let tp1 = makeTypeStr typeof<Shared4.MenuServerMsg.MainMenuMsgs>
    let tp2 = makeTypeStr typeof<Shared4.GameRoomMsg.ParentGameRoomMsg>
    let tp3 = makeTypeStr typeof<GameTypes.Msg>
    let tp4 = makeTypeStr typeof<GameServerTypes.GameNotification> 
    let tp5 = makeTypeStr typeof<GameTypes.RegisterInLobby>

    override __.FromBinary(bytes,_ttype) =
        let typeInfo = bytes.[0..39] |> Array.takeWhile ((<>) (byte 0)) |> Encoding.UTF8.GetString
        
        bytes.[40..]
        |>
        (if typeInfo = tp1 then 
            fromBytes<Shared4.MenuServerMsg.MainMenuMsgs>
        elif typeInfo = tp2 then
            fromBytes<Shared4.GameRoomMsg.ParentGameRoomMsg>
        elif typeInfo = tp3 then
            fromBytes<GameTypes.Msg>
        elif typeInfo = tp4 then
            fromBytes<GameServerTypes.GameNotification> 
        elif typeInfo = tp5 then
            fromBytes<GameTypes.RegisterInLobby>
        else 
            fromBytes<Shared4.LobbyMsg.LobbyMsg>)
        

type LauncherSerializer(system) = 
    inherit MySerializer(system)

    override this.FromBinary(bytes,_ttype) = fromBytes<Shared4.LauncherMsg.LauncherMsgs> bytes.[40..]
        
type DecoderState = {
    parsedChars : char list
    bracketCount : int
    parsingUserInput : bool
}
    with 
        static member Init() = {
            parsedChars = []
            bracketCount = 0
            parsingUserInput = false
        }

        

let serializeType (object : Shared4.Messages.TcpMsgs) = 
    object 
    |> JsonConvert.SerializeObject 
    |> Encoding.UTF8.GetBytes
    

let fetchMsgs<'T> (input : string) =
    
    let stringReader = new System.IO.StringReader(input)
    let reader = new JsonTextReader(stringReader)
    reader.SupportMultipleContent <- true

    let rec loop state = 
        if reader.Read() && reader.TokenType = JsonToken.StartObject  then
           
            try
                let obj = JObject.Load(reader)
                
                let x : 'T = downcast obj.ToObject(typeof<'T>)

                let newState = x :: fst state , reader.LinePosition 

                loop newState

            with
            | e -> 
                printf "Invalid Json procedure!!!"
                fst state,Some <| snd state
        else
            fst state,None

    let msgs,lastValid = loop ([],0)

    let rest =
        match lastValid with
        | Some x -> input.[x..]
        | _ -> ""
         
    (msgs |> List.rev,rest)

let deserializeBytes bytes  =
    let stringified = 
        bytes
        |> Encoding.UTF8.GetString

    fetchMsgs stringified
   

let tcpReader onMsg (stream : NetworkStream) name = 

    let buffer : byte [] = Array.zeroCreate 256
    
    let rec loop (read: StringBuilder) =

        try

            let readCnt = stream.Read(buffer,0,256)

            let (msgs,rest) = 
                buffer.[0..readCnt - 1]
                |> Encoding.UTF8.GetString
                |> sprintf "%s%s"(read.ToString())
                |> fetchMsgs

            List.iter onMsg msgs

            read.Clear() |> ignore

            rest
            |> read.Append 
            |> loop

        with
        | err -> 
            let a = name
            Shared4.Messages.usD 
            |> Shared4.Messages.TcpMsgs.SystemMsgs 
            |> onMsg
        
    StringBuilder()
    |> loop
    |> ignore
    

    
     

   
        
    


    