module App

open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared
open AuthUtil
open MongoDB.FSharp
open Newtonsoft.Json
open Serialization.Json
open Shared4.MenuServerMsg
open Shared4.HttpTypes
open Config

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us


let webApp = router {
    post "/api/login" (fun next ctx ->
        task {
            let! loginModel = ctx.BindJsonAsync<Registered>()
            let! u = DbStorage.MongoI.getUserByName loginModel.nickName

            let u = u.Value

            if Crypto.Pbkdf2.verify u.PasswordHash loginModel.passWord then

                let tokenRes = generateToken loginModel.nickName [||] |> Success
                
                return! json tokenRes next ctx
            else
                return! json Failure next ctx
        }
    )
    get "/api/init" (fun next ctx ->

        task {
            let counter = {Value = 42}
            return! json counter next ctx
        })

    getf "/api/user?Name=%s" (fun userName -> (fun next ctx ->
        task {
            match! DbStorage.MongoI.getUserByName userName with
            | Some usr ->
                return! json usr next ctx
            | _ -> 
                return! text "klops" next ctx
        }))
    post "/api/user" (fun next ctx ->
            task {
                let! u = ctx.BindModelAsync<RegistrationRequest>()

                let passwordHash = u.Password |> Crypto.Pbkdf2.strongHash

                let user = {
                    Name = u.Nick
                    DateOfRegistration = System.DateTime.UtcNow
                    PasswordHash = passwordHash
                }

                let! x = DbStorage.MongoI.insertNewUser user

                let regResult = 
                    match x with
                    | Ok() -> RegisteredOk
                    | Error(x) -> x.Message |> Klops
               
                return! json regResult next ctx
            }
    ) 
}

let app = application {
    url ("http://192.168.0.107:" + port.ToString() + "/")
    my_own_jwt_auth Secret
    use_router webApp
    memory_cache
    use_static publicPath
    //use_json_serializer(JsonSerializer())
    use_gzip
}

//Serializers.Register()

run app
