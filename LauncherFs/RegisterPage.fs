module RegisterPage

open Elmish
open Elmish.WPF
open System
open Shared4.Common

type Model = {
    NickName : string
    Password : string
    ConfirmPassword : string
    Email : string
    ErrorMsg : string option
}

type Msg = 
    | NickNameChanged of string
    | PasswordChanged of string
    | ConfirmPasswordChanged of string
    | Email of string
    | Submit
    | Cancel
    | RegistrationProblem of string

let init() = 
    {NickName = "";Password = "";ConfirmPassword = "";Email = "";ErrorMsg = None},Cmd.none

let update msg model =
    let newModel = 
        match msg with
        | NickNameChanged x -> {model with NickName = x}
        | PasswordChanged x -> {model with Password = x}
        | ConfirmPasswordChanged x -> {model with ConfirmPassword = x}
        | Email x -> {model with Email = x}
        | Submit -> model
        | RegistrationProblem msg -> {model with ErrorMsg = Some msg}
        | _ -> failwith ""

    newModel,Cmd.none