module DbStorage

module MongoI = 

    open MongoDB.Driver
    open MongoDB.Bson
    open MongoDB.FSharp
    open System
    open Microsoft.FSharp.Reflection
    open Shared4.HttpTypes
    open Config

    type User = {
        Id : BsonObjectId
        Name : string
        DateOfRegistration : DateTime
        PasswordHash : string
    }

   
    let private awaitTaskToResult task = async {
        let! res = 
            task 
            |> Async.AwaitTask
            |> Async.Catch
        match res with
        | Choice1Of2 r -> return Ok(r)
        | Choice2Of2 err -> return Error(err)
    }

    let private convertToMongoType<'T,'R> (rawType: 'R) = 
        let vals = FSharpValue.GetRecordFields(rawType)

        let (id : obj) = upcast BsonObjectId.Create(ObjectId.GenerateNewId())

        FSharpValue.MakeRecord(typeof<'T>,Array.append [|id|] vals) :?> 'T         

    let private convertFromMongoType<'T,'R> (mongoType : 'R) =
        let vals = FSharpValue.GetRecordFields(mongoType)

        FSharpValue.MakeRecord(typeof<'T>,Array.tail vals) :?> 'T 

    let connect() =
        let client = MongoClient(ConnectionString)
        client.GetDatabase("wisedev")
        

    let private getCollection<'a> collectionName = 
        let db = connect()
        db.GetCollection<'a> collectionName

    let getUsersCollection() = getCollection<User> "Users"

    let insertNewUser user =
        let mongoUser = convertToMongoType<User,Shared4.HttpTypes.User> user
        let users = getUsersCollection()
        async {
            let! r =
                 users.InsertOneAsync mongoUser 
                 |> Async.AwaitTask
                 |> Async.Catch
            match r with
            | Choice1Of2 () ->
                return Ok(())
            | Choice2Of2 ex -> return Error ex
        }

    let getUserByName userName = 
        let users = getUsersCollection()
        let filter = Builders<User>
                        .Filter
                        .Eq((fun user -> user.Name),userName)
        async {
            let! task = users.FindAsync filter |> awaitTaskToResult
            
            match task with
            | Ok cursor ->
                let! rr = cursor.SingleAsync<User>() |> awaitTaskToResult
                match rr with
                | Ok user ->
                    return Some(user |> convertFromMongoType<Shared4.HttpTypes.User,User>)
                | _ -> return None
            | _ -> return None
                
        }
    

        
        
    

