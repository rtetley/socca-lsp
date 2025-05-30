type 'a log = Log : 'a -> 'a log

val mk_log : string -> (?force:bool -> (unit -> string) -> unit) log
val logs : unit -> string list

type event = string
type events = event Sel.Event.t list

val lsp_initialization_done : unit -> events

(* debug messages coming from either the language server or the client *)
val debug : event Sel.Event.t