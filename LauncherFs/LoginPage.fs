module LoginPage

open Elmish
open Elmish.WPF

type Model = {
    nick : string
    password : string
    isLogging : bool
    infoMsg : string option
}

type Msg =
    | PasswordChanged of string
    | NickChanged of string
    | Login
    | Register
    | RegisterCompleted
    | WrongCreditentials
    | ServerError

let update msg model : Model*Cmd<Msg> =
    let newModel = 
        match msg with
        | PasswordChanged x -> {model with password = x}
        | NickChanged x -> {model with nick = x}
        | RegisterCompleted -> {model with infoMsg = Some "Zarejestrowano Pomyslnie!"}
        | WrongCreditentials -> {model with isLogging = false;infoMsg = Some "Zle dane Logowania !"}
        | ServerError -> {model with isLogging = false;infoMsg = Some "Blad Serwera Przepraszamy!"}
        | _ -> model

    newModel,Cmd.none

let init() = ({
    nick = ""
    password = ""
    isLogging = false
    infoMsg = None
 } , Cmd.none)

let bindings () : Binding<Model,Msg> list = 
    [
        "InfoMsg" |> Binding.oneWayOpt (function {infoMsg = x} -> x)
        "Nick" |> Binding.twoWay((fun ({nick = n}) -> n), NickChanged)
        "Password" |> Binding.twoWay((fun ({password = n}) -> n), PasswordChanged)
        "Login" |> Binding.cmd Login
        "Register" |> Binding.cmd Register
        "CanLogin" |> Binding.oneWay (function | {isLogging = x} -> not x)
        "LoginText" |> Binding.oneWay (function | {isLogging = true} -> "Logowanie..." | _ -> "Zaloguj sie")
    ]
