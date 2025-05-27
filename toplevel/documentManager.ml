type loc = Lexing.position

module Host = struct
  type ast = string
  let parse ?start (text:string): (ast * loc) option =
    match String.split_on_char ' ' text with
    | [] -> None
    | s :: _ ->
      let ast = s in 
      let loc =
        match start with
        | None -> Lexing.dummy_pos
        | Some start -> Lexing.{ start with pos_cnum = start.pos_cnum + String.length s }
      in
      Some (ast, loc)
end

type parse_state = { raw_doc: string; loc : loc; doc : Host.ast list }

type document = {
  raw_doc : string; (* TODO: use real raw doc *)
  cancel_handle: Sel.Event.cancellation_handle option;
}

type dm_event = 
| ParseBegin
| ParseEvent of parse_state
| Invalidate of parse_state

let pp_dm_event fmt = function
 | ParseEvent _ -> Format.fprintf fmt "ParseEvent _"
 | Invalidate _ -> Format.fprintf fmt "Invalidate _"
 | ParseBegin -> Format.fprintf fmt "ParseBegin"

let handle_parse_event (st : parse_state) =
  match Host.parse ~start:st.loc st.raw_doc with
  | Some (ast, loc) ->
    ParseEvent { st with doc = st.doc @ [ast]; loc }
  | None ->
    (* finished *)
    Invalidate st

let handle_parse_begin ({ raw_doc; cancel_handle } as document) =
  (* Cancel any previous parsing event *)
  Option.iter Sel.Event.cancel cancel_handle;
  let parsed_state = {
    raw_doc;
    loc = Lexing.dummy_pos;
    doc = []
  } in
  let priority = Some Common.PriorityManager.parsing in
  let event = Sel.now ?priority (ParseEvent parsed_state) in
  let cancel_handle = Some (Sel.Event.get_cancellation_handle event) in
  {document with cancel_handle}, [event]

let doValidate st = assert false

let handle_dm_event doc e =
  match e with
  | ParseEvent st ->
    let priority = Some Common.PriorityManager.parsing in
    [Sel.now ?priority (handle_parse_event st)]
  | Invalidate st -> doValidate st
  | ParseBegin ->
    let _, e = handle_parse_begin doc in
    e