module UserMenu

open Elmish
open Elmish.WPF

type Model = Nothing

type Msg = 
    | GoToProfile
    | GoToInventory
    | GoToSettings

let bindings () : Binding<Model,Msg> list  = [
    "GoToProfile" |> Binding.cmd(GoToProfile)
    "GoToInventory" |> Binding.cmd(GoToInventory)
    "GoToSettings" |> Binding.cmd(GoToSettings)
]