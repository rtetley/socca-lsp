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

let log f = ()

let loop () =
  let events = LspManager.init () in
  let rec loop (todo : LspManager.event Sel.Todo.t) =
    (*log fun () -> "looking for next step";*)
    flush_all ();
    let ready, todo = Sel.pop todo in
    let nremaining = Sel.Todo.size todo in
    log (fun () -> Format.asprintf "Main loop event ready: %a, %d events waiting\n\n" LspManager.pr_event ready nremaining);
    log (fun () -> "==========================================================");
    log (fun () -> Format.asprintf "Todo events: %a" (Sel.Todo.pp LspManager.pr_event) todo );
    log (fun () -> "==========================================================\n\n");
    let new_events = LspManager.handle_event ready in
    let todo = Sel.Todo.add todo new_events in
    log (fun () -> "==========================================================");
    log (fun () -> Format.asprintf "New Todo events: %a" (Sel.Todo.pp LspManager.pr_event) todo );
    log (fun () -> "==========================================================\n\n");
    loop todo
  in
  let todo = Sel.Todo.add Sel.Todo.empty events in
  try loop todo
  with exn -> assert false

let () =
  Sys.(set_signal sigint Signal_ignore);
  loop ()
