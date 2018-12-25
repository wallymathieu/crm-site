namespace Crm
open Crm.Domain

open System.IO
open System
open Newtonsoft.Json

type JsonAppendToFile(fileName) =
  let notNull= not << isNull
  let fileDoesNotExist = not << File.Exists
  do
    if fileDoesNotExist fileName then File.WriteAllText(fileName, "")

  interface IAppendBatch with
    member __.Batch cs = async{
      use fs = File.Open(fileName, FileMode.Append, FileAccess.Write, FileShare.Read)
      use w = new StreamWriter(fs)
      let json = JsonConvert.SerializeObject (Array.ofList cs)
      do! w.WriteLineAsync(json)
      do! fs.FlushAsync()
      return ()
    }
    member __.ReadAll() = async{
      use fs = File.Open(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)
      use r = new StreamReader(fs)
      let! lines = r.ReadToEndAsync()
      let map line= JsonConvert.DeserializeObject<(CommandContext*Command) array> line
      let splitLines (s:string)=s.Split([|'\r';'\n'|], StringSplitOptions.RemoveEmptyEntries)
      return splitLines lines
              |> Array.map map
              |> Array.concat
              |> Array.toList
              |> List.sortBy (fst >> CommandContext.at)
    }
