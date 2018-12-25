namespace Crm
open Crm.Domain

type IAppendBatch =
  abstract Batch : (CommandContext*Command) list -> Async<unit>
  abstract ReadAll : unit -> Async<(CommandContext*Command) list>
