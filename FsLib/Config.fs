module Config

let configString2 = sprintf """
    akka {
        actor {
            provider = remote
            serializers {
               json = "Serializer+GameSerializer, FsLib"
            }
            serialization-bindings {
                 "System.Object" = json
           }
        }
       
        remote {
            dot-netty.tcp {
                port = 8091
                hostname = localhost
            }
        }
    }
       """
let configString = configString2