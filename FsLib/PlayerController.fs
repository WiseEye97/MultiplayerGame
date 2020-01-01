module PlayerController

open Godot
open SharedTypes.Common

type Keys = 
    | KeyLeft
    | KeyRight
    | KeyDown 
    | KeyUp 
    with 
        member this.GetStr() = 
            match this with
            | KeyLeft -> "ui_left"
            | KeyRight -> "ui_right"
            | KeyDown -> "ui_down"
            | KeyUp -> "ui_up"

        static member GetAllKeysStrs() = [KeyLeft;KeyRight;KeyDown;KeyUp] |> List.map (fun x -> x.GetStr() |> Input.IsActionPressed)


module Helpers = 
    let (|CenterHorizontal|_|) = 
        function
        | [true;true;_;_] -> Some ()
        | _ -> None

    let (|CenterVertical|_|) = 
        function
        | [_;_;true;true] -> Some ()
        | _ -> None

    let (|MoveLeft|MoveRight|DoNotMoveHorizontal|) = 
           function
           | [true;false;_;_] -> MoveLeft
           | [false;true;_;_] -> MoveRight
           | _ -> DoNotMoveHorizontal

    let (|MoveUp|MoveDown|DoNotMoveVertical|) = 
        function
        | [_;_;false;true] -> MoveUp
        | [_;_;true;false] -> MoveDown
        | _ -> DoNotMoveVertical
    
    let (|PositiveF|NegativeF|ZeroF|) =
        function
        | x when x < 0.0f -> NegativeF
        | 0.0f -> ZeroF
        | _ -> PositiveF

    let (|GoingRight|GoingLeft|GoingUp|GoingDown|Idle|) (v : Vector3) = 
        match v.x,v.z with
        | PositiveF,ZeroF -> GoingLeft
        | NegativeF,ZeroF -> GoingRight
        | ZeroF,PositiveF -> GoingUp
        | ZeroF,NegativeF -> GoingDown
        | ZeroF,ZeroF -> Idle
        | x -> x |> sprintf "Invalid State of vector %A" |> failwith

module Vector = 
    open Helpers
    open System

    let zero = Vector3(0.0f,0.0f,0.0f)

    let vecCenter = Vector3(0.0f,0.0f,0.0f)
    let vecLeft = Vector3(5.0f,0.0f,0.0f)
    let vecRight = Vector3(-5.0f,0.0f,0.0f)
    let vecUp = Vector3(0.0f,0.0f,5.0f)
    let vecDown = Vector3(0.0f,0.0f,-5.0f)

    let slowDown v =
           match v with
           | GoingRight -> Vector3(Math.Min(5.0f,v.x + 0.1f),0.0f,0.0f)
           | GoingLeft -> Vector3(Math.Max(0.0f,v.x - 0.1f),0.0f,0.0f)
           | GoingUp -> Vector3(0.0f,0.0f,Math.Max(0.0f,v.z - 0.1f))
           | GoingDown -> Vector3(0.0f,0.0f,Math.Min(5.0f,v.z + 0.1f))
           | Idle -> v

    let applyEvent v = 
        function
        | GoLeft -> vecLeft
        | GoRight -> vecRight
        | GoUp -> vecUp
        | GoDown -> vecDown
        | Stop -> vecCenter
        | SlowDown -> slowDown v

    let private matchKeyState = 
        function
        | CenterHorizontal | CenterVertical -> Stop
        | MoveLeft -> GoLeft
        | MoveRight -> GoRight
        | MoveUp -> GoUp
        | MoveDown -> GoDown
        | _ -> SlowDown

    let genEvent() = 
        Keys.GetAllKeysStrs () |> matchKeyState
        

