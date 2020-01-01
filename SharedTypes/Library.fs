namespace SharedTypes

module Common = 
    type PlayerEvent = 
        | GoLeft
        | GoRight
        | GoUp
        | GoDown
        | Stop
        | SlowDown

module GameServerTypes =
    type GameNotification = 
        | RegisterPlayer of string
        | MoveEvent of Common.PlayerEvent

module GameTypes =
    
    type RegisterInLobby = {
        name : string
        token : string
    }

    type OpponentInfo = {
        pos : float32*float32
        name : string
    }

    type GameCommands = 
        | SpawnOpponent of OpponentInfo
        | SetMyPosition of float32*float32
        | OpponentMove of OpponentInfo * Common.PlayerEvent
        | RegisterPlayer of string
        | NoTask

    type InitialFromLauncher = {
        serverIndex : int
        lobbyToken : string
        myName : string
    }
    
    type Msg = 
        | InitialFromLauncher of InitialFromLauncher
        | LobbyResolved of string*string
        | GameCommands of GameCommands
        | ToServer of GameServerTypes.GameNotification
    


    