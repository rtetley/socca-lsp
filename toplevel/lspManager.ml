(**************************************************************************)
(*                                                                        *)
(*                                 VSRocq                                  *)
(*                                                                        *)
(*                   Copyright INRIA and contributors                     *)
(*       (see version control and README file for authors & dates)        *)
(*                                                                        *)
(**************************************************************************)
(*                                                                        *)
(*   This file is distributed under the terms of the MIT License.         *)
(*   See LICENSE file.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** This toplevel implements an LSP-based server language for VsCode,
    used by the VsRocq extension. *)


open Lsp.Types

let Common.Log.Log log = Common.Log.mk_log "lspManager"

type error = {
  code: Jsonrpc.Response.Error.Code.t option;
  message: string;
}

type packed = Pack : 'a Lsp.Client_request.t -> packed

type state = {
  document : Dm.DocumentManager.document;
}

type tab = {
  st : state;
  visible : bool
}

let states : (string, tab) Hashtbl.t = Hashtbl.create 39

let conf_request_id = max_int

let server_info = InitializeResult.create_serverInfo
  ~name:"vscoq-language-server"
  ~version:"2.2.6"
  ()

type lsp_event = 
  | Receive of Jsonrpc.Packet.t option
  | Send of Jsonrpc.Packet.t

type event =
 | LspManagerEvent of lsp_event
 | DocumentManagerEvent of DocumentUri.t * Dm.DocumentManager.dm_event
 (* | Notification of notification *)
 | LogEvent of Common.Log.event

type events = event Sel.Event.t list

let lsp : event Sel.Event.t =
  Sel.On.httpcle ~priority:Common.PriorityManager.lsp_message ~name:"lsp" Unix.stdin (function
    | Ok buff ->
      begin
        log (fun () -> "UI req ready");
        try LspManagerEvent (Receive (Some (Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string (Bytes.to_string buff)))))
        with exn ->
          log (fun () -> "failed to decode json");
          LspManagerEvent (Receive None)
      end
    | Error exn ->
        log (fun () -> ("failed to read message: " ^ Printexc.to_string exn));
        (* do not remove this line otherwise the server stays running in some scenarios *)
        exit 0)


let output_json obj =
  let msg  = Yojson.Safe.pretty_to_string ~std:true obj in
  let size = String.length msg in
  let s = Printf.sprintf "Content-Length: %d\r\n\r\n%s" size msg in
  log (fun () -> "sent: " ^ msg);
  ignore(Unix.write_substring Unix.stdout s 0 (String.length s)) (* TODO ERROR *)

let output_notification notif = output_json @@ Jsonrpc.Notification.yojson_of_t @@ Lsp.Server_notification.to_jsonrpc notif
let send_configuration_request () =
  let id = `Int conf_request_id in
  let mk_configuration_item section =
    ConfigurationItem.({ scopeUri = None; section = Some section })
  in
  let items = List.map mk_configuration_item ["vsrocq"] in
  let req = Lsp.Server_request.(to_jsonrpc_request (WorkspaceConfiguration { items }) ~id) in
  Send (Request req)

(*
let inject_em_event x = Sel.Event.map (fun e -> ExecutionManagerEvent e) x
let inject_em_events events = List.map inject_em_event events *)
let inject_dm_event uri x = Sel.Event.map (fun e -> DocumentManagerEvent (uri, e)) x
let inject_dm_events uri events = List.map (inject_dm_event uri) events

let inject_debug_event x : event Sel.Event.t =
  Sel.Event.map (fun x -> LogEvent x) x
let inject_debug_events l =
  List.map inject_debug_event l

let do_initialize id params =
  let Lsp.Types.InitializeParams.{ initializationOptions } = params in
  begin match initializationOptions with
  | None -> log (fun () -> "Failed to decode initialization options")
  | Some initializationOptions -> ()
  end;
  let textDocumentSync = `TextDocumentSyncKind TextDocumentSyncKind.Incremental in
  let completionProvider = CompletionOptions.create ~resolveProvider:false () in
  let documentSymbolProvider = `DocumentSymbolOptions (DocumentSymbolOptions.create ~workDoneProgress:true ()) in
  let hoverProvider = `Bool true in
  let definitionProvider = `Bool true in
  let capabilities = ServerCapabilities.create
    ~textDocumentSync
    ~completionProvider
    ~hoverProvider
    ~definitionProvider
    ~documentSymbolProvider
  ()
  in
  let initialize_result = Lsp.Types.InitializeResult.{
    capabilities = capabilities; 
    serverInfo = Some server_info;
  } in
  log (fun () -> "---------------- initialized --------------");
  let debug_events = Common.Log.lsp_initialization_done () |> inject_debug_events in
  Ok initialize_result, debug_events@[Sel.now @@ LspManagerEvent (send_configuration_request ())]

let do_shutdown id params =
  Ok(()), []

let do_exit () =
  exit 0

let parse_loc json =
  let open Yojson.Safe.Util in
  let line = json |> member "line" |> to_int in
  let character = json |> member "character" |> to_int in
  Position.{ line ; character }

let publish_diagnostics uri doc =
  let diagnostics = [] in
  let params = Lsp.Types.PublishDiagnosticsParams.create ~diagnostics ~uri () in
  let diag_notification = Lsp.Server_notification.PublishDiagnostics params in
  output_notification (diag_notification)

let send_error_notification message =
  let type_ = MessageType.Error in
  let params = ShowMessageParams.{type_; message} in
  let notification = Lsp.Server_notification.ShowMessage params in
  output_json @@ Jsonrpc.Notification.yojson_of_t @@ Lsp.Server_notification.to_jsonrpc notification

let update_view uri (st:state) = publish_diagnostics uri st

let replace_state path st visible = Hashtbl.replace states path { st; visible}

let open_new_document uri text =
  let document, event = Dm.DocumentManager.create_document text in
  let st = { document } in
  Hashtbl.add states (DocumentUri.to_path uri) { st ; visible = true; };
  update_view uri st;
  [inject_dm_event uri event]

let textDocumentDidOpen params =
  let Lsp.Types.DidOpenTextDocumentParams.{ textDocument = { uri; text } } = params in
  match Hashtbl.find_opt states (DocumentUri.to_path uri) with
  | None -> open_new_document uri text
  | Some { st } ->
    update_view uri st;
    []

let textDocumentDidChange params =
  let Lsp.Types.DidChangeTextDocumentParams.{ textDocument; contentChanges } = params in
  let uri = textDocument.uri in
  match Hashtbl.find_opt states (DocumentUri.to_path uri) with
    | None -> log (fun () -> "[textDocumentDidChange] ignoring event on non-existing document"); []
    | Some { st; visible } ->
      let mk_text_edit TextDocumentContentChangeEvent.{ range; text } =
        Option.get range, text
      in
      let text_edits = List.map mk_text_edit contentChanges in
      let document, events = Dm.DocumentManager.apply_text_edits st.document text_edits in
      let st = { document } in
      replace_state (DocumentUri.to_path uri) st visible;
      update_view uri st;
      inject_dm_events uri events

let textDocumentDidSave params =
  [] (* TODO handle properly *)

let current_memory_usage () =
  let { Gc.heap_words; _ } = Gc.stat () in
  Sys.word_size * heap_words

let purge_invisible_tabs () =
  Hashtbl.filter_map_inplace (fun u ({ visible } as v) ->
    if visible then Some v
    else begin
      log (fun () -> "purging tab " ^ u);
      None
    end)
  states

let consider_purge_invisible_tabs () =
  let usage = current_memory_usage () in
  if usage > !Config.max_memory_usage (* 4G *) then begin
    purge_invisible_tabs ();
    Gc.compact ();
    let new_usage = current_memory_usage () in
    log (fun () -> Printf.sprintf  "memory footprint %d -> %d" usage new_usage);
  end

let textDocumentDidClose params =
  let Lsp.Types.DidCloseTextDocumentParams.{ textDocument } = params in
  let path = DocumentUri.to_path textDocument.uri in
  begin match Hashtbl.find_opt states path with
  | None -> log (fun () -> "[textDocumentDidClose] closed document with no state")
  | Some { st } -> replace_state path st false
  end;
  consider_purge_invisible_tabs ();
  [] (* TODO handle properly *)

let textDocumentHover id params = 
  let Lsp.Types.HoverParams.{ textDocument; position } = params in
  let open Yojson.Safe.Util in
  match Hashtbl.find_opt states (DocumentUri.to_path textDocument.uri) with
  | None -> log (fun () -> "[textDocumentHover] ignoring event on non existing document"); Ok None (* FIXME handle error case properly *)
  | Some { st } -> Ok None (* FIXME handle error case properly *)

let textDocumentDefinition params =
  let Lsp.Types.DefinitionParams.{ textDocument; position } = params in
  match Hashtbl.find_opt states (DocumentUri.to_path textDocument.uri) with
  | None -> log (fun () -> "[textDocumentDefinition] ignoring event on non existing document"); Ok None (* FIXME handle error case properly *)
  | Some { st } -> Ok None


let progress_hook uri () =
  match Hashtbl.find_opt states (DocumentUri.to_path uri) with
  | None -> log (fun () -> "ignoring non existent document")
  | Some { st } -> update_view uri st

let documentSymbol id params =
  let Lsp.Types.DocumentSymbolParams.{ textDocument = {uri}; partialResultToken; workDoneToken } = params in (*TODO: At some point we might get ssupport for partialResult and workDone*)
  match Hashtbl.find_opt states (DocumentUri.to_path uri) with
  | None -> log (fun () -> "[documentSymbol] ignoring event on non existent document"); Error({message="Document does not exist"; code=None}), []
  | Some tab -> log (fun () -> "[documentSymbol] getting symbols"); Ok None, []

let dispatch_std_request : type a. Jsonrpc.Id.t -> a Lsp.Client_request.t -> (a, error) result * events =
  fun id req ->
  match req with
  | Initialize params ->
    do_initialize id params
  | Shutdown ->
    do_shutdown id ()
  (* | TextDocumentCompletion params ->
    textDocumentCompletion id params *)
  | TextDocumentDefinition params ->
    textDocumentDefinition params, []
  | TextDocumentHover params ->
    textDocumentHover id params, []
  | DocumentSymbol params ->
    documentSymbol id params
  | UnknownRequest _ | _  -> Error ({message="Received unknown request"; code=None}), []

let dispatch_std_notification = 
  let open Lsp.Client_notification in function
  | TextDocumentDidOpen params -> log (fun () -> "Received notification: textDocument/didOpen");
    textDocumentDidOpen params
  | TextDocumentDidChange params -> log (fun () -> "Received notification: textDocument/didChange");
    textDocumentDidChange params
  | TextDocumentDidClose params ->  log (fun () -> "Received notification: textDocument/didClose");
    textDocumentDidClose params
  (* | ChangeConfiguration params -> log (fun () -> "Received notification: workspace/didChangeConfiguration");
    workspaceDidChangeConfiguration params *)
  | Initialized -> []
  | Exit ->
    do_exit ()
  | UnknownNotification _ | _ -> log (fun () -> "Received unknown notification"); []

(* let dispatch_notification =
  let open Notification.Client in function
  | InterpretToPoint params -> log (fun () -> "Received notification: prover/interpretToPoint"); rocqtopInterpretToPoint params
  | InterpretToEnd params -> log (fun () -> "Received notification: prover/interpretToEnd"); rocqtopInterpretToEnd params
  | StepBackward params -> log (fun () -> "Received notification: prover/stepBackward"); rocqtopStepBackward params
  | StepForward params -> log (fun () -> "Received notification: prover/stepForward"); rocqtopStepForward params
  | Std notif -> dispatch_std_notification notif *)

let handle_lsp_event = function
  | Receive None -> [lsp]
  | Receive (Some rpc) ->
    lsp :: (* the event is recurrent *)
    begin try
      let json = Jsonrpc.Packet.yojson_of_t rpc in
      let msg = Yojson.Safe.pretty_to_string ~std:true json in
      log (fun () -> "received: " ^ msg);
      begin match rpc with
      | Request req ->
          log (fun () -> "ui request: " ^ req.method_);
          begin match Lsp.Client_request.of_jsonrpc req with
          | Error(e) -> log (fun () -> "Error decoding request: " ^ e); []
          | Ok(Lsp.Client_request.E r) ->
            let resp, events = dispatch_std_request req.id r in
            begin match resp with
            | Error {code; message} ->
              let code = Jsonrpc.Response.Error.Code.RequestFailed in
              output_json @@ Jsonrpc.Response.(yojson_of_t @@ error req.id (Error.make ~code ~message ()))
            | Ok resp ->
              let resp = Lsp.Client_request.yojson_of_result r resp in
              output_json @@ Jsonrpc.Response.(yojson_of_t @@ ok req.id resp)
            end;
            events
          end
      | Notification notif ->
        begin match Lsp.Client_notification.of_jsonrpc notif with
        | Ok notif -> dispatch_std_notification notif
        | Error e -> log (fun () -> "error decoding notification: " ^ e); []
        end
      | Response resp ->
          log (fun () -> "got unknown response");
          []
      | Batch_response _ -> log (fun () -> "Unsupported batch response received"); []
      | Batch_call _ -> log (fun () -> "Unsupported batch call received"); []
      end
    with Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error(exn,json) ->
      log (fun () -> "error parsing json: " ^ Yojson.Safe.pretty_to_string json);
      []
    end
  | Send jsonrpc ->
    output_json (Jsonrpc.Packet.yojson_of_t jsonrpc); []

let pp_lsp_event fmt = function
  | Receive jsonrpc ->
    Format.fprintf fmt "Request"
  | Send jsonrpc ->
    Format.fprintf fmt "Send"

type handled_event = {
    state : state option;
    events: event Sel.Event.t list;
    update_view: bool;
    notification: Lsp.Server_notification.t option;
}

let handle_event ev =
  match ev with
  | LspManagerEvent e -> handle_lsp_event e
  | DocumentManagerEvent (uri, e) ->
    begin match Hashtbl.find_opt states (DocumentUri.to_path uri) with
    | None ->
      log (fun () -> "ignoring event on non-existing document");
      []
    | Some { st; visible } ->
      let document, events, result = Dm.DocumentManager.handle_dm_event st.document e in
      begin match result with
      | None ->
        let state = { document } in
        replace_state (DocumentUri.to_path uri) state visible;
        inject_dm_events uri events
      | Some new_doc ->
        let state = { document = new_doc } in
        replace_state (DocumentUri.to_path uri) state visible;
        update_view uri st;
        []
      end
    end
  (* | Notification notification ->
    begin match notification with 
    | QueryResultNotification params ->
      output_notification @@ SearchResult params; [inject_notification Bm.SearchQuery.query_feedback]
    end *)
  | LogEvent e ->
    (* send_rocq_debug e; *) [inject_debug_event Common.Log.debug]

let pp_event fmt = function
  | LspManagerEvent e -> pp_lsp_event fmt e
  | DocumentManagerEvent (_, e) -> Dm.DocumentManager.pp_dm_event fmt e
  (* | Notification _ -> Format.fprintf fmt "notif" *)
  | LogEvent _ -> Format.fprintf fmt "debug"

let init () =
  [lsp]
