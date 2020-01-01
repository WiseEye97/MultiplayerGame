module ProfileViewer

open Elmish
open Elmish.WPF
open System
open Shared4.Common

type PersonRelationShip = 
    | IsMe
    | IsFriend
    | Other

type LoadedModel = {
    nick : string
    joinDate : DateTime
    relationShip : PersonRelationShip 
}

type Model = 
    | NotLoaded
    | Loaded of LoadedModel

    with
        member this.GetNick() = 
            match this with
            | NotLoaded -> "Loading..."
            | Loaded l -> l.nick

        member this.GetJoinDate() = 
            match this with
            | NotLoaded -> "Loading..."
            | Loaded l -> l.joinDate.ToLongDateString()

        member this.GetFriendButtonText() =
            match this with
            | NotLoaded -> "Loading..."
            | Loaded l -> 
                match l.relationShip with
                | IsMe -> ""
                | IsFriend -> "Wyrzuc ze znajomych"
                | Other -> "Zapros"

        member this.CanClickButton() = 
            match this with
            | Loaded {relationShip = Other} -> true
            | _ -> false

type Msg = 
    | InfoLoaded of PlayerProfileInfo
    | CloseProfile

let private getPersonInfoTask personName = async {
    return {
        PlayerProfileInfo.nick = personName
        PlayerProfileInfo.joinDate = DateTime.Now
        PlayerProfileInfo.isFriend = false
    }
}

let init (personName : string) : Model*Cmd<Msg> = 
    NotLoaded,Cmd.OfAsync.perform getPersonInfoTask personName InfoLoaded

let update isSelf msg model = 
    match msg,model with 
    | InfoLoaded info,NotLoaded ->
        
        let newModel = Loaded {
            nick = info.nick
            joinDate = info.joinDate
            relationShip = if isSelf then IsMe elif info.isFriend then IsFriend else Other
        }

        newModel,Cmd.none
    | InfoLoaded _,_ ->
        failwith "Invalid Model State"
