open Suave
open System

open Crm.Web
open Crm.Domain
open Crm
open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Operators
type CmdArgs =
  { IP : System.Net.IPAddress
    Port : Sockets.Port
    Json : string option
  }
[<EntryPoint>]
let main argv =
  // parse arguments
  let args =
    let (|Port|_|) : _-> UInt16 option = tryParse
    let (|IPAddress|_|) :_->System.Net.IPAddress option= tryParse

    //default bind to 127.0.0.1:8083
    let defaultArgs =
      { IP = System.Net.IPAddress.Loopback
        Port = 8083us
        Json = None
      }

    let rec parseArgs b args =
      match args with
      | [] -> b
      | "--ip" :: IPAddress ip :: xs -> parseArgs { b with IP = ip } xs
      | "--port" :: Port p :: xs -> parseArgs { b with Port = p } xs
      | "--json" :: file :: xs -> parseArgs { b with Json = Some file } xs
      | invalidArgs ->
        printfn "error: invalid arguments %A" invalidArgs
        printfn "Usage:"
        printfn "    --ip ADDRESS   ip address (Default: %O)" defaultArgs.IP
        printfn "    --port PORT    port (Default: %i)" defaultArgs.Port
        exit 1

    argv
    |> List.ofArray
    |> parseArgs defaultArgs

  let repository = ContactRepository()
  let appenders = Seq.toList (seq {
        if Option.isSome args.Json then yield JsonAppendToFile(args.Json.Value) :> IAppendBatch
      })

  let commands = monad.plus {
                  for appender in appenders do
                    yield appender.ReadAll()
                 }
                 |> Async.Parallel |> Async.RunSynchronously
                 |> Seq.collect id
                 |> Seq.map snd
                 |> Seq.toList
  for command in commands do
    repository.Handle command |> ignore
  let time ()=DateTime.UtcNow
  let append c =async {
    for appender in appenders do
      do! appender.Batch([ c ]) }
  let webPart = webPart repository append time
  // start suave
  startWebServer { defaultConfig with bindings = [ HttpBinding.create HTTP args.IP args.Port ] } (OptionT.run << webPart )
  0
