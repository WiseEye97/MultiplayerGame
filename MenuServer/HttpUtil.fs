
module HttpUtil

open System.Text
open System.IO
open System.Net
open Newtonsoft.Json
open FSharp.Data
open Shared4.MenuServerMsg

[<Literal>]
let apiUrl = "http://192.168.0.107:8085"

let sendPostRequest ob onSuccess onError = 
    async {
        let url = apiUrl
        
        let ser = JsonConvert.SerializeObject ob

        // Create & configure HTTP web request
        let req = HttpWebRequest.Create(url) :?> HttpWebRequest 
        req.ProtocolVersion <- HttpVersion.Version10
        req.Method <- "POST"
        
        // Encode body with POST data as array of bytes
        let postBytes = Encoding.ASCII.GetBytes(ser)
        req.ContentType <- "application/json";
        req.ContentLength <- int64 postBytes.Length
        // Write data to the request
        use! reqStream = req.GetRequestStreamAsync() |> Async.AwaitTask

        do! reqStream.WriteAsync(postBytes, 0, postBytes.Length) |> Async.AwaitTask

        reqStream.Close()
        
        // Obtain response and download the resulting page 
        // (The sample contains the first & last name from POST data)
        let! resp = req.GetResponseAsync() |> Async.AwaitTask
        let stream = resp.GetResponseStream() 
        let reader = new StreamReader(stream) 
        let! res = reader.ReadToEndAsync() |> Async.AwaitTask

        return (onSuccess res)
    }

let sendReqToken data token = 
    let ser = JsonConvert.SerializeObject data

    async {
        return! Http.AsyncRequestString
               ( apiUrl,
                 headers = [ "Content-Type", HttpContentTypes.Json;HttpRequestHeaders.Authorization(sprintf "Bearer %s" token) ],
                 body = TextRequest ser) |>  Async.Catch
    }

let sendGetReq (data : 'a option) token = 
    let ser = JsonConvert.SerializeObject data

    async {
          return! Http.AsyncRequestString
                 ( apiUrl,
                   headers = [ "Content-Type", HttpContentTypes.Json;HttpRequestHeaders.Authorization(sprintf "Bearer %s" token) ],
                   body = TextRequest ser) |>  Async.Catch
    }

let authenticate (info : Registered) onSuccess onError = 
    let ser = JsonConvert.SerializeObject info

    async {
           let! res = Http.AsyncRequestString
                          ( sprintf "%s/api/login" apiUrl,
                            headers = [ "Content-Type", HttpContentTypes.Json],
                            httpMethod = HttpMethod.Post,
                            body = TextRequest ser) |>  Async.Catch
           let r = match res with
                   | Choice1Of2 x -> 
                        try
                            let r = JsonConvert.DeserializeObject<Shared4.HttpTypes.AuthResult>(x)
                            r |> onSuccess
                        with
                        | x ->
                            printfn "%A" x.Message
                            failwith ""
                   | Choice2Of2 err -> err |> onError

           return r
    }

let createUser (req : Shared4.HttpTypes.RegistrationRequest) onSuccess onError = 
    let ser = JsonConvert.SerializeObject req

    async {
            let! res = Http.AsyncRequestString
                            ( sprintf "%s/api/user" apiUrl,
                            headers = [ "Content-Type", HttpContentTypes.Json],
                            httpMethod = HttpMethod.Post,
                            body = TextRequest ser) |>  Async.Catch
            let r = match res with
                    | Choice1Of2 x -> JsonConvert.DeserializeObject<Shared4.HttpTypes.RegistrationResult>(x) |> onSuccess
                    | Choice2Of2 err -> err |> onError

            return r
    }


   
    
