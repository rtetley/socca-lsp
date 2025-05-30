type 'a log = Log : 'a -> 'a log

val mk_log : string -> (?force:bool -> (unit -> string) -> unit) log
val logs : unit -> string list
