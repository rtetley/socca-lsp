type dm_event
type document

val handle_dm_event : document -> dm_event -> document * dm_event Sel.Event.t list * document option

val pp_dm_event : Format.formatter -> dm_event -> unit

val create_document : string -> document * dm_event Sel.Event.t

val apply_text_edits : document -> RawDocument.text_edit list -> document * dm_event Sel.Event.t list
(** [apply_text_edits doc edits] updates the text of [doc] with [edits]. The new
    text is not parsed or executed. *)