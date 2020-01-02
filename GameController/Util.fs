module Util

open Akka.IO
open System.Text

let fetchMsgsWithBuffer<'T> (data : ByteString) buffer = 
    let msgStr = data.ToArray() |> Encoding.ASCII.GetString

    let msgStr = String.concat "" [buffer;msgStr]

    Serializer.fetchMsgs<'T> msgStr