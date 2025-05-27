let max_memory_usage  = ref 4000000000

let full_diagnostics = ref false
let full_messages = ref false

type options = {
  enableDiagnostics : bool
}

let default_options = {
  enableDiagnostics = true;
}

let options = ref default_options

let set_options o = options := o
let set_default_options () = options := default_options

let is_diagnostics_enabled () = !options.enableDiagnostics

let get_options () = !options
