open System

open Crm.Web
open Crm.Domain
open Crm

open FSharpPlus
open FSharpPlus.Data

open FSharpPlus.AspNetCore
open FSharpPlus.AspNetCore.HttpAdapter

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Builder

type CmdArgs =
  { Json : string option }
[<EntryPoint>]
let main argv =
  // parse arguments
  let args =
    let (|Port|_|) : _-> UInt16 option = tryParse
    let (|IPAddress|_|) :_->System.Net.IPAddress option= tryParse

    //default bind to 127.0.0.1:8083
    let defaultArgs =
      { Json = None }

    let rec parseArgs b args =
      match args with
      | [] -> b
      | "--json" :: file :: xs -> parseArgs { b with Json = Some file } xs
      | invalidArgs ->
        printfn "error: invalid arguments %A" invalidArgs
        printfn "Usage:"
        printfn "    --json                 FILE        path to filename to store commands"
        exit 1

    argv
    |> List.ofArray
    |> parseArgs defaultArgs

  let repository = ContactRepository()
  let appenders = Seq.toList (seq {
    if Option.isSome args.Json then yield JsonAppendToFile(args.Json.Value) :> IAppendBatch
  })

  let commands =
      monad.plus {
        for appender in appenders do
          yield appender.ReadAll()
      }
      |> Async.Parallel
      |> Async.map Array.toList
      |> Async.RunSynchronously
      |> List.collect (List.map snd)

  for command in commands do
    repository.Handle command |> ignore
  let time ()=DateTime.UtcNow
  let append c =async {
    for appender in appenders do
      do! appender.Batch([ c ]) }

  let onCommand (context,command) = async {
    do! append(context, command)
    return repository.Handle command
  }

  let buildWebHost args =
    WebHost.CreateDefaultBuilder(args)
      .Configure(fun (app: IApplicationBuilder)->
        let authenticated = ``x-jwt-payload authenticated``
        let webPart = webPart authenticated repository onCommand time
        app
            |> appMap "/v1/" (Suave.appRun webPart)
            |> ignore
      ).Build()
  buildWebHost(argv).Run()
  0
