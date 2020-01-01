namespace Shared

open System

type LoginInfo = {
    nickName : string
    passWord : string
}

type Counter = { Value : int }

type RegistrationResult = 
    | RegisteredOk
    | Klops of string

type NewUserRequest = {
    Nick : string
    Password : string
    Mail : string
}


