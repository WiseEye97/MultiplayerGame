module Common

open Akka.Actor

type WPFMsg =
  | LoginMsg of LoginPage.Msg
  | MenuMsg of Menu.Msg
  | RegisterActor of IActorRef
  | FromServer of Shared4.LauncherMsg.MsgToUi
  | LobbyM of Lobby.Msg
  | InitLobby of Shared4.LauncherMsg.MyLobbyInfo
  | ProfileViewerMsg of ProfileViewer.Msg
  | RegistrationMsg of RegisterPage.Msg

