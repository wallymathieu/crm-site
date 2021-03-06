[<AutoOpen>]
module Crm.AsyncExtensions
open System
open System.Threading.Tasks
// From: https://github.com/SuaveIO/suave/blob/e200d29d115ee4a62368310551b58938954f67cb/src/Suave/Utils/AsyncExtensions.fs

type Microsoft.FSharp.Control.AsyncBuilder with
  /// An extension method that overloads the standard 'Bind' of the 'async' builder. The new overload awaits on
  /// a standard .NET task
  member x.Bind(t : Task<'T>, f:'T -> Async<'R>) : Async<'R> = async.Bind(Async.AwaitTask t, f)

  /// An extension method that overloads the standard 'Bind' of the 'async' builder. The new overload awaits on
  /// a standard .NET task which does not commpute a value
  member x.Bind(t : Task, f : unit -> Async<'R>) : Async<'R> = async.Bind(Async.AwaitTask t, f)

