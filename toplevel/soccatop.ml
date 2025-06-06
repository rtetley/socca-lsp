(**************************************************************************)
(*                                                                        *)
(*                             Socca-lsp                                  *)
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

(** This toplevel implements an LSP-based server template for languages in OCaml. *)

let Common.Log.Log log = Common.Log.mk_log "soccatop"

let loop () =
  let events = LspManager.init () in
  let rec loop (todo : LspManager.event Sel.Todo.t) =
    (*log fun () -> "looking for next step";*)
    flush_all ();
    let ready, todo = Sel.pop todo in
    let nremaining = Sel.Todo.size todo in
    log (fun () -> Format.asprintf "Main loop event ready: %a, %d events waiting\n\n" LspManager.pp_event ready nremaining);
    log (fun () -> "==========================================================");
    log (fun () -> Format.asprintf "Todo events: %a" (Sel.Todo.pp LspManager.pp_event) todo );
    log (fun () -> "==========================================================\n\n");
    let new_events = LspManager.handle_event ready in
    let todo = Sel.Todo.add todo new_events in
    log (fun () -> "==========================================================");
    log (fun () -> Format.asprintf "New Todo events: %a" (Sel.Todo.pp LspManager.pp_event) todo );
    log (fun () -> "==========================================================\n\n");
    loop todo
  in
  let todo = Sel.Todo.add Sel.Todo.empty events in
  try loop todo
  with exn ->
    log ~force:true (fun () -> Format.asprintf "%s@." (Printexc.get_backtrace ()))

let () =
  (* HACK to be compatible with the current vsrocq extension *)
  if Array.length (Sys.argv) > 1 then
    if Sys.argv.(1) = "-where" then (Format.printf "/tmp@."; exit 0)
    else if Sys.argv.(1) = "-v" then (
      Format.printf "The Coq Proof Assistant, version 8.20.1\ncompiled with OCaml 5.3.0@."; exit 0);
  (* end of HACK *)
  (* HACK: to be sure backtraces are recorded *)
  Printexc.record_backtrace true;
  Sys.(set_signal sigint Signal_ignore);
  loop ()
