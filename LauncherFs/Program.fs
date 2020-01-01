// Learn more about F# at http://fsharp.org

open System
open System.Windows.Controls
open Elmish.WPF
open Elmish
open Akka
open Akka.FSharp
open Common
open Akka.Actor
open System.Windows
open Shared4.LauncherMsg
open Shared4.MenuServerMsg

type LoginControl = LoginControl of LanucherView.Pages.Login
type MenuControl = MenuControl of LanucherView.Pages.MenuPage
type LobbyControl = LobbyControl of LanucherView.Pages.Lobby

type PageModel = 
    | MenuPage of Menu.Model * MenuControl
    | LobbyPage of Lobby.LobbyClientModel * LobbyControl
    with
        member this.GetControl() : UserControl =
            match this with
            | MenuPage (_,MenuControl c) -> upcast c
            | LobbyPage (_,LobbyControl c) -> upcast c
                

type ServerRequest<'Model,'Msg> = {
    onReply : 'Model * Confirmation -> ('Model*Cmd<'Msg>) option
}

type InitialModel = {
    Content : LanucherView.DefaultPage
}   
    with
        static member Init() = {
            Content = LanucherView.DefaultPage()
        }

type UserInfo = {
    nick : string
    token : string
}

type RegisteredAdditionalWindow = 
    | ProfileViewer of ProfileViewer.Model
    | InventoryViewer


type Model = 
    | Initial of InitialModel
    | Connected of Connected
    | Registered of Registered

and 
    Connected =
      { 
        LoginPage : LoginPage.Model * LoginControl
        ActorRef : Shared4.Common.ActorRefWrapper<FromUser>
        RegistrationWindow : RegisterPage.Model option
        NetworkStatus : NetworkStatus
      }
        with
            member this.GetActivePage() = this.LoginPage
and
    Registered = {
        Page : PageModel
        ActorRef : Shared4.Common.ActorRefWrapper<FromUser>
        additionalWindow : RegisteredAdditionalWindow option
        info : UserInfo
        NetworkStatus : NetworkStatus
    }
      with
        member this.GetActivePage() = this.Page

and
   NetworkStatus =
        | Idle
        | WaitingForReply of ServerRequest<Model,Common.WPFMsg> 

let init _ () = Initial <| InitialModel.Init(),[]

module Helpers = 
    let (|InMenu|_|) = 
        function
        | (Registered ({Page = MenuPage (menuModel,con)} as reg) ) ->
            Some (menuModel,con,reg)
        | _ -> None

    let (|InLobby|_|) = 
        function
        | (Registered  ({Page = LobbyPage (lobbyModel,con)} as reg) ) ->
            Some (lobbyModel,con,reg)
        | _ -> None

    let (|DuringLogging|_|) = 
        function
        | (Connected ({LoginPage = (loginP,control)} as connnected)) -> Some(loginP,control,connnected)
        | _ -> None

    let (|ConnectedAndWaiting|_|) = 
        function
        | (Connected ({NetworkStatus = WaitingForReply {onReply = fn}} as model) ) -> Some (fn,model)
        | _ -> None

    let (|RegisteredAndWaiting|_|) = 
           function
           | (Registered ({ NetworkStatus = WaitingForReply {onReply = fn}} as model)) -> Some (fn,model)
           | _ -> None

    let (|OpenSelfProfileViewer|_|) = 
        function
        | MenuMsg (Menu.Msg.UserMenuMsg UserMenu.Msg.GoToProfile),InMenu(_,_,registered) -> Some registered
        | _ -> None

    let (|ProfileViewerOpened|_|) = 
        function
        | Registered {additionalWindow = Some (ProfileViewer md)} -> Some (Some md)
        | _ -> None

    let (|OpenRegistrationForm|_|) = 
        function
        | LoginMsg (LoginPage.Msg.Register),DuringLogging(_,_,conn) -> Some (conn)
        | _ -> None
    
    let (|RegistrationFormOpened|_|) = 
        function
        | Connected ({RegistrationWindow = Some x} as c) -> Some (x,c)
        | _ -> None

    let (|CloseRegistrationForm|_|) = 
           function
           | RegistrationMsg RegisterPage.Msg.Cancel,RegistrationFormOpened(_,conn) -> Some (conn)
           | _ -> None

    let (|FromRegistrationPage|_|) x = 
        match x with
        | RegistrationMsg m,RegistrationFormOpened (a,c) -> Some(m,c,a)
        | _ -> None

    let (|SubmitRegistration|_|) = 
        function
        | RegistrationMsg (RegisterPage.Submit),RegistrationFormOpened (a,c) ->
            let req = {
                Shared4.HttpTypes.RegistrationRequest.Nick = a.NickName
                Shared4.HttpTypes.Password = a.Password
                Shared4.HttpTypes.Mail = a.Email
            }

            c.ActorRef.dispatch (CreateNewUser req)

            Some()
        | _ -> None
open Helpers

let onLogin = 
    function
    | Connected conn, Logged info ->
        let p = conn.GetActivePage() |> fst
        let (menuModel,menuCmd) = Menu.init (info,conn.ActorRef)

        let registeredModel = {
            Page = MenuPage (menuModel,MenuControl <| LanucherView.Pages.MenuPage())
            ActorRef = conn.ActorRef;additionalWindow = None
            info =  {nick = p.nick;token = ""}
            NetworkStatus = Idle}

        Some(Registered registeredModel,Cmd.map MenuMsg menuCmd)

    | Connected conn, WrongCreditenials -> 
        let (l,c) = LoginPage.update LoginPage.Msg.WrongCreditentials (conn.LoginPage |> fst)
        Some(Connected {conn with LoginPage = (l,conn.LoginPage |> snd)},Cmd.map LoginMsg c)

    | Connected conn, ServerProblem ->
        let (l,c) = LoginPage.update LoginPage.Msg.ServerError (conn.LoginPage |> fst)
        Some(Connected {conn with LoginPage = (l,conn.LoginPage |> snd)},Cmd.map LoginMsg c)

    | _ -> None

let onLobbyCreated  = 
    function
    | (InMenu (menuModel,con,reg)) , LobbyCreated x ->
        let newMenuModel = {menuModel with CreateLobbyWindow = None}

        Some(Registered {reg with Page = MenuPage (newMenuModel,con);NetworkStatus = Idle},Cmd.ofMsg (InitLobby x))
    | _ -> None

let onLobbyEntered  = 
    function
    | (InMenu (_,_,reg)),JoinedLobby info ->
        let (lobbyModel,lobbyCmd) = Lobby.init(info.msgs,info.users,MainActor.actorLobbyWrapper reg.ActorRef.actorRef,reg.info.nick)
     
        Some (Registered {reg with Page = LobbyPage (lobbyModel,LobbyControl <| LanucherView.Pages.Lobby());NetworkStatus = Idle},Cmd.map LobbyM lobbyCmd)
    | _ -> None

let onMsgEmmited =
    function
    | (InMenu (menuModel,con,reg)),ChatMsgDelivered -> 
        let (newMenuModel,menuCmd) = Menu.update (reg.info.nick |> Menu.UserName |> Menu.MessageSend |> Menu.ConfirmationFromServer) menuModel

        Some(Registered {reg with  Page = MenuPage (newMenuModel,con) ;NetworkStatus = Idle},Cmd.map MenuMsg menuCmd)
    | _ -> None
   
let onMsgEmmitedLobby = 
    function
    | InLobby (lobbyModel,lobbyControl,reg),ChatMsgDelivered -> 
        let (newModel,newCmd) = Lobby.update (Lobby.Msg.ServerConfirmations Lobby.Confirmations.MessageDeliveried) lobbyModel
        
        Some(Registered {reg with Page = LobbyPage (newModel,lobbyControl) ;NetworkStatus = Idle},Cmd.map LobbyM newCmd)
    | _ -> None

let onLobbyLeft  = 
    function
    | InLobby (_,_,reg),BackToMenu info ->
        let (menuModel,menuCmd) = Menu.init (info,reg.ActorRef)
       
        Some(Registered {reg with NetworkStatus = Idle; Page = MenuPage (menuModel,MenuControl <| LanucherView.Pages.MenuPage())},Cmd.map MenuMsg menuCmd)
    | _ -> None

let onRegistered = 
    function
    | RegistrationFormOpened (_,c),Confirmation.Registered ->
        let (newLoginModel,loginCmd) = LoginPage.update LoginPage.Msg.RegisterCompleted (fst c.LoginPage)
        Some(Connected {c with RegistrationWindow = None;LoginPage = (newLoginModel,c.LoginPage |> snd)},Cmd.map LoginMsg loginCmd)
    | RegistrationFormOpened (a,c),Confirmation.NotRegistered ->
        let (a,b) = RegisterPage.update (RegisterPage.Msg.RegistrationProblem "Cant Register") a
        Some(Connected {c with RegistrationWindow = Some a},Cmd.map RegistrationMsg b)
    | RegistrationFormOpened (a,c),Confirmation.RegisteredServerError ->
        let (a,b) = RegisterPage.update (RegisterPage.Msg.RegistrationProblem "Server Error Sorry :(") a
        Some(Connected {c with RegistrationWindow = Some a},Cmd.map RegistrationMsg b)
    | _ -> None

let update (actorSys : ExtendedActorSystem) (msg : WPFMsg) model =
    
    let (|ToCatch|_|) = 
        function
        | LoginMsg (LoginPage.Msg.Login) , DuringLogging(loginP,loginControl,conn)->
            let toSend = {nickName = loginP.nick;passWord = loginP.password} |> RegisterUser

            conn.ActorRef.dispatch toSend

            let newLoginModel = {loginP with isLogging = true}

            Some (Connected {conn with LoginPage = (newLoginModel,loginControl) ;NetworkStatus = WaitingForReply {onReply = onLogin}},[])
        | OpenRegistrationForm conn ->

              let (initialModel,cmd) = RegisterPage.init()

              Some (Connected {conn with RegistrationWindow = Some initialModel},Cmd.map RegistrationMsg cmd)
        | ProfileViewerMsg (ProfileViewer.CloseProfile),(Registered ({additionalWindow = Some _} as reg)) -> Some(Registered {reg with additionalWindow = None},Cmd.none)
        | _ -> None

    let (|CatchAndForward|_|) = 
        function
        | MenuMsg (Menu.Msg.CreateLobbyMsg Menu.LobbyCreator.CreateCommand as msg) , InMenu (menuModel,menuControl,reg) ->
            
            let (newMenuModel,menuCmd) = Menu.update msg menuModel
        
            Some (Registered {reg with Page = MenuPage (newMenuModel,menuControl) ;NetworkStatus = WaitingForReply {onReply = onLobbyCreated}} ,Cmd.map MenuMsg menuCmd)

        | MenuMsg (Menu.Msg.EmitChatMsg _ as msg) , InMenu(menuModel,menuControl,reg) ->

            let (newMenuModel,menuCmd) = Menu.update msg menuModel
            
            Some (Registered {reg with Page = MenuPage (newMenuModel,menuControl) ;NetworkStatus = WaitingForReply {onReply = onMsgEmmited}} ,Cmd.map MenuMsg menuCmd)

        | MenuMsg (Menu.Msg.JoinLobby _ as msg) , InMenu(menuModel,menuControl,reg)  ->

            let (newMenuModel,menuCmd) = Menu.update msg menuModel
           
            Some (Registered {reg with Page = MenuPage (newMenuModel,menuControl) ;NetworkStatus = WaitingForReply {onReply = onLobbyEntered}} ,Cmd.map MenuMsg menuCmd)

        | LobbyM (Lobby.Msg.EmitChatMsg _ as msg),InLobby (lobbyModel,lobbyControl,reg) ->

            let (newModel,newCmd) = Lobby.update msg lobbyModel

            Some (Registered {reg with Page = LobbyPage (newModel,lobbyControl) ;NetworkStatus = WaitingForReply {onReply = onMsgEmmitedLobby}} ,Cmd.map LobbyM newCmd)

        | LobbyM (Lobby.Msg.Leave),InLobby (lobbyModel,lobbyControl,reg) ->
            let (newModel,newCmd) = Lobby.update Lobby.Msg.Leave lobbyModel
      
            Some (Registered {reg with Page = LobbyPage (newModel,lobbyControl) ;NetworkStatus = WaitingForReply {onReply = onLobbyLeft}} ,Cmd.map LobbyM newCmd)

        | ProfileViewerMsg (ProfileViewer.InfoLoaded info),ProfileViewerOpened (Some ProfileViewer.NotLoaded) ->

               let (reg,myName) = model |> function | Registered ({info = {nick = n}} as reg) -> reg,n | _ -> failwith "never gonna happen"
               let isSelf = info.nick = myName

               let (newPModel,newPCmd) = ProfileViewer.update isSelf (ProfileViewer.InfoLoaded info) ProfileViewer.NotLoaded

               Some(Registered {reg with additionalWindow = Some (ProfileViewer newPModel)},Cmd.map ProfileViewerMsg newPCmd)

        | ProfileViewerMsg m,ProfileViewerOpened (Some (ProfileViewer.Loaded {nick = n} as x)) ->
               
            let (reg,myName) = model |> function | Registered ({info = {nick = n}} as reg) -> reg,n | _ -> failwith "never gonna happen"
            let isSelf = n = myName

            let (newPModel,newPCmd) = ProfileViewer.update isSelf m x
               
            Some(Registered {reg with additionalWindow = Some (ProfileViewer newPModel)},Cmd.map ProfileViewerMsg newPCmd)

        | CloseRegistrationForm c -> Some(Connected {c with RegistrationWindow = None},Cmd.none)

        | RegistrationMsg RegisterPage.Submit,RegistrationFormOpened (a,c) ->

            let req = {
                Shared4.HttpTypes.Nick = a.NickName
                Shared4.HttpTypes.Password = a.Password
                Shared4.HttpTypes.Mail = a.Email
            }

            let (regModel,regCmd) = RegisterPage.update RegisterPage.Submit a

            c.ActorRef.dispatch (CreateNewUser req)

            Some(Connected {c with RegistrationWindow = Some regModel;NetworkStatus = WaitingForReply {onReply = onRegistered}},Cmd.map RegistrationMsg regCmd)
            
        | _ -> None

    let handleNotifications = 
        function
        | NewLobby lobyyInfo,InMenu(menuModel,menuControl,reg)  ->
            let menuModel,menuCmd = Menu.update (lobyyInfo |> Menu.AddNewLobby |> Menu.NotificationsFromServer) menuModel
            
            Registered {reg with Page = MenuPage (menuModel,menuControl)} ,Cmd.map MenuMsg menuCmd

        | LobbyNotification notif,InLobby (lobbyModel,lobbyControl,reg) ->
            let lobbyModel,lobbyCmd = Lobby.update (notif |> Lobby.Msg.ServerNotifications) lobbyModel
            
            Registered {reg with Page = LobbyPage (lobbyModel,lobbyControl)} ,Cmd.map LobbyM lobbyCmd

        | ServerChanged lobyyInfo ,InMenu(menuModel,menuControl,reg)  ->
            let menuModel,menuCmd = Menu.update (lobyyInfo |> Menu.ServerEdited |> Menu.NotificationsFromServer) menuModel
            
            Registered {reg with Page = MenuPage (menuModel,menuControl)} ,Cmd.map MenuMsg menuCmd

        | GlobalChatMsg info,InMenu(menuModel,menuControl,reg) ->
            let msg = 
                info 
                |> Menu.toNewChatMsg 
                |> Menu.Msg.NotificationsFromServer

            let menuModel,menuCmd = Menu.update msg menuModel

            Registered {reg with Page = MenuPage (menuModel,menuControl)} ,Cmd.map MenuMsg menuCmd
        | DeleteServers servers,InMenu(menuModel,menuControl,reg) ->

            let toDelete = servers |> Seq.map (fun x -> x.LobbyId) |> Set.ofSeq
            let newServers = menuModel.GameList |> List.filter (fun x -> toDelete.Contains x.LobbyId |> not)
            let newMenuModel = {menuModel with GameList = newServers}

            Registered {reg with Page = MenuPage (newMenuModel,menuControl)} ,Cmd.none

    let handleFromServer =
        function 
        | Notification notif,x -> 
            handleNotifications(notif,x) 
        | Confirmation conf , ConnectedAndWaiting (fn,model) -> 
            match fn (Connected model,conf) with
            | Some x -> x
            | _ -> Connected model,Cmd.none
        | Confirmation conf , RegisteredAndWaiting (fn,model) -> 
            match fn (Registered model,conf) with
            | Some x -> x
            | _ ->Registered model,Cmd.none
        | _ ->
            failwith "Unknown Server Message"
            
    match msg,model with
    | ToCatch c -> c

    | CatchAndForward c -> c

    | OpenSelfProfileViewer reg ->

        let (profModel,profCmd) = ProfileViewer.init(reg.info.nick)
        let newModel = Registered {reg with additionalWindow = Some (ProfileViewer profModel)}
        newModel,Cmd.map ProfileViewerMsg profCmd

    | InitLobby (MyLobbyInfo ({lobbyRef = refString} as info)),InMenu (_,_,reg) ->
        
        let lobbyRef = actorSys.Provider.ResolveActorRef(refString)
        reg.ActorRef.actorRef <! (SystemMsg (AddLobbyActor lobbyRef))

        let (lobbyModel,lobbyCmd) = Lobby.init(info.msgs,info.users,MainActor.actorLobbyWrapper reg.ActorRef.actorRef,reg.info.nick)

        Registered {reg with Page = LobbyPage (lobbyModel,LobbyControl <| LanucherView.Pages.Lobby());NetworkStatus = Idle},Cmd.map LobbyM lobbyCmd

    | FromRegistrationPage (m,conn,x) ->
          let (newModel,cmd) = RegisterPage.update m x
          Connected {conn with RegistrationWindow = Some newModel},Cmd.map RegistrationMsg cmd
             
    | LoginMsg msg,DuringLogging (loginP,control,con) -> 
        let loginModel,loginCmd = LoginPage.update msg loginP
        Connected {con with LoginPage = (loginModel,control)},Cmd.map LoginMsg loginCmd

    | MenuMsg msg,InMenu (menuModel,menuControl,reg) ->
        let menuModel,menuCmd = Menu.update msg menuModel
      
        Registered {reg with Page = MenuPage (menuModel,menuControl)},Cmd.map MenuMsg menuCmd

    | LobbyM msg,InLobby (lobbyModel,lobbyControl,reg) ->
        let newModel,newCmd = Lobby.update msg lobbyModel
 
        Registered {reg with Page = LobbyPage (newModel,lobbyControl)},Cmd.map LobbyM newCmd

    | RegisterActor actor , Initial _ ->
        Connected {
            LoginPage = (LoginPage.init() |> fst , LoginControl <| LanucherView.Pages.Login())
            ActorRef = MainActor.actorWrapper actor
            NetworkStatus = Idle
            RegistrationWindow = None
        },[]
    | FromServer msg,model ->
        handleFromServer (msg,model)
    | _ ->
        failwith "Pattern Matching Failed"
        


let bindings () : Binding<Model, WPFMsg> list =
  [
    "LoginPage" |> Binding.subModelOpt(
        (function | Connected {LoginPage = (p,_)} -> Some p | _ -> None),
        snd,
        LoginMsg,
        LoginPage.bindings)
    "MenuPage" |> Binding.subModelOpt(
        (function | Registered ({Page = MenuPage (p,_)}) -> Some p | _ -> None),
        snd,
        MenuMsg,
        Menu.bindings)
    "LobbyPage" |> Binding.subModelOpt(
           (function | Registered {Page = LobbyPage (p,_)} -> Some p | _ -> None),
           snd,
           LobbyM,
           Lobby.bindings)
    "Content" |> Binding.oneWay (function
        | Initial m -> 
            let content : UserControl = upcast m.Content
            content
        | Connected {LoginPage = (_,(LoginControl c))} -> upcast c
        | Registered {Page = p} -> p.GetControl()
    )
    "ProfileViewer" |> Binding.subModelWin(
        ((function | ProfileViewerOpened x -> x | _ -> None) >> WindowState.ofOption),
        ProfileViewerMsg,
        (fun () -> [
            "PersonNick" |> Binding.oneWay(fun (_,(x : ProfileViewer.Model)) -> x.GetNick())
            "JoinDate" |> Binding.oneWay(fun (_,(x : ProfileViewer.Model)) -> x.GetJoinDate())
            "FriendActionText" |> Binding.oneWay(fun (_,(x : ProfileViewer.Model)) -> x.GetFriendButtonText())
            "CanJoin" |> Binding.oneWay(fun (_,(x : ProfileViewer.Model)) -> x.CanClickButton())
            "ClosingWindow" |> Binding.cmd ProfileViewer.CloseProfile
        ]),
        (fun () -> LanucherView.WWindows.ProfileViewer(Owner = Application.Current.MainWindow))
    )
    "RegisterWindow" |> Binding.subModelWin(
        ((function | RegistrationFormOpened x -> Some (fst x) | _ -> None) >> WindowState.ofOption),
        RegistrationMsg,
        (fun () -> [
            "Nick" |> Binding.twoWay((fun (_,x : RegisterPage.Model) -> x.NickName), RegisterPage.NickNameChanged)
            "Email"  |> Binding.twoWay((fun (_,x : RegisterPage.Model) -> x.Email), RegisterPage.Email)
            "Password"  |> Binding.twoWay((fun (_,x : RegisterPage.Model) -> x.Password), RegisterPage.PasswordChanged)
            "PasswordC"  |> Binding.twoWay((fun (_,x : RegisterPage.Model) -> x.ConfirmPassword), RegisterPage.ConfirmPasswordChanged)
            "ErrorMsg" |> Binding.oneWay((fun (_,x : RegisterPage.Model) -> x.ErrorMsg |> Option.defaultValue ""))
            "Submit"  |> Binding.cmd RegisterPage.Submit
            "Cancel"  |> Binding.cmd RegisterPage.Cancel
        ]),
        (fun () -> LanucherView.WWindows.RegisterWindow(Owner = Application.Current.MainWindow))
    )
  ]


[<EntryPoint; STAThread>]
let main _ =
    
    use system = System.create "menu-server-system" (Configuration.parse(Actors.configString2))
    
    let actorSystem : ExtendedActorSystem = downcast system  

    let mainActor = Actors.createWPFActor system None MainActor.initial MainActor.update Actors.emptyOther
    
    let window : Windows.Window = upcast LanucherView.MainWindow()  

    Program.mkProgramWpf (init actorSystem) (update actorSystem) bindings
    |> Program.withSubscription (fun _ -> Cmd.ofSub (mainActor >> ignore))
    |> Program.runWindow window 
