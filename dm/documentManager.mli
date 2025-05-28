type dm_event
type document

val handle_dm_event : document -> dm_event -> dm_event Sel.Event.t list * document option

val pp_dm_event : Format.formatter -> dm_event -> unit