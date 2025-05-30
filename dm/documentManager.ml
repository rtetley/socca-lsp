type loc = Lsp.Types.Position.t

let Common.Log.Log log = Common.Log.mk_log "documentManager"

module Host = struct
  (* for now, dummy implementations *)
  
  (* type of base blocks *)
  type ast = string

  (* parse as much as it wants *)
  let parse ?start (stream:char Stream.t): (ast * loc) option =
    let rec aux acc =
      match Stream.next stream with
      | exception Stream.Failure (* end of file *) -> None
      | ' ' ->
        let nb_newline = List.length (String.split_on_char '\n' acc) in
        let nb_char = String.length acc - String.rindex acc '\n' in
        let loc =
          match start with
          | None -> Lsp.Types.Position.{ character = nb_char; line = nb_newline }
          | Some start ->
            Lsp.Types.Position.{ character = start.character + nb_char; line = start.line + nb_newline }
          in
        Some (acc, loc)
      | c ->
        aux (String.make 1 c^acc)
    in
    aux ""

end

module LM = Map.Make (Int)

type document = {
  sentences_by_end : Host.ast LM.t;
  parsed_loc : int;
  raw_doc : RawDocument.t;
  cancel_handle: Sel.Event.cancellation_handle option;
}

type parse_state = {
  started: float;
  stop: int;
  loc: Lsp.Types.Position.t option;
  stream: char Stream.t;
  raw: RawDocument.t;
  parsed: Host.ast list;
  previous_document: document;
}

type dm_event = 
| ParseBegin
| ParseMore of parse_state
| ParseEnd of parse_state

let pp_dm_event fmt = function
 | ParseBegin -> Format.fprintf fmt "ParseBegin"
 | ParseMore _ -> Format.fprintf fmt "ParseMore _"
 | ParseEnd _ -> Format.fprintf fmt "ParseEnd _"

let loc_after_sentence = function
  | Some (stop, _) -> stop
  | None -> -1

let loc_strictly_before parsed pos =
  loc_after_sentence @@
    LM.find_last_opt (fun stop -> stop < pos) parsed.sentences_by_end

let handle_parse_begin ({ parsed_loc; raw_doc; cancel_handle } as document) =
  (* Cancel any previous parsing event *)
  Option.iter Sel.Event.cancel cancel_handle;
  (* We take the state strictly before parsed_loc to cover the case when the
  end of the sentence is editted *)
  let stop = loc_strictly_before document parsed_loc in
  let text = RawDocument.text raw_doc in
  let stream = Stream.of_string text in
  while Stream.count stream < stop do Stream.junk stream done;
  log (fun () -> Format.sprintf "Parsing more from pos %i" stop);
  let started = Unix.gettimeofday () in
  let parsed_state = {stop; stream; raw=raw_doc; parsed=[]; loc=None; started; previous_document=document} in
  let priority = Some Common.PriorityManager.parsing in
  let event = Sel.now ?priority (ParseMore parsed_state) in
  let cancel_handle = Some (Sel.Event.get_cancellation_handle event) in
  {document with cancel_handle}, [event]

let handle_parse_more (st : parse_state) : dm_event =
  match Host.parse ?start:st.loc st.stream with
  | Some (ast, loc) ->
    ParseMore { st with parsed = st.parsed @ [ast]; loc = Some loc }
  | None ->
    (* finished *)
    ParseEnd st

(* let validate (st:parse_state) (doc:document) : document =
  doc *)

let pos_at_end parsed =
  match LM.max_binding_opt parsed.sentences_by_end with
  | Some (stop, _) -> stop
  | None -> -1

let handle_parse_end {started} (doc:document) : document =
  let end_ = Unix.gettimeofday ()in
  let time = end_ -. started in
  log (fun () -> Format.sprintf "Parsing phase ended in %5.3f\n%!" time);
  let parsed_loc = pos_at_end doc in
  let parsed_document = {doc with parsed_loc} in
  parsed_document

let handle_dm_event doc e : document * dm_event Sel.Event.t list * document option =
  match e with
  | ParseBegin ->
    let document, events = handle_parse_begin doc in
    document, events, None
  | ParseMore st ->
    let priority = Some Common.PriorityManager.parsing in
    let event = handle_parse_more st in
    doc, [Sel.now ?priority event], None
  | ParseEnd st ->
    let valid_doc = handle_parse_end st doc in
    doc, [], Some valid_doc

let create_document text =
  let raw_doc = RawDocument.create text in
  let doc =
    { 
      parsed_loc = -1;
      raw_doc;
      sentences_by_end = LM.empty;
      cancel_handle = None;
    }
  in
  let priority = Some Common.PriorityManager.launch_parsing in
  let event = Sel.now ?priority ParseBegin in
  doc, event