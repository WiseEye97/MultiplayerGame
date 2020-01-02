module TcpTypes

open Newtonsoft
open Newtonsoft.Json.Linq
open Newtonsoft.Json

type Test = 
    | A
    | B


type PlayerInfo = {
    nick : string
    game_id : int
}

type NotifyPlayerMove = {
    player_id : int
    new_pos_x : int
    new_pos_y : int
}

type MenuServerMsg = 
    | RegisterNewGame

type ClientMsg = 
    | PlayerInfo of PlayerInfo

type ServerMsg = 
    | NotifyPlayerMove


let nothing = ()