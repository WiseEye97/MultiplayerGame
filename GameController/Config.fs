module Config

let configString = """
    akka {
        actor {
            provider = remote
            serializers {
               json = "Serializer+MenuSerializer, Shared4"
            }
            serialization-bindings {
                 "System.Object" = json
           }
        }
       
        remote {
            dot-netty.tcp {
                port = 8090
                hostname = 192.168.0.105
            }
        }
    }
           """