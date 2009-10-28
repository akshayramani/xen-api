(** Bindings to the License Policy Engine (LPE) C-code *)

(** Citrix proprietary code *)
val _proprietary_code_marker : string

(** Initialise LPE and check out license *)
external initialise :
  string -> int -> string -> string -> string -> bool * bool * int * int
  = "initialise_c"
  
(** Release license, and shut down and clean up LPE *)
external shutdown : unit -> bool = "shutdown_c"

(** Set the PID of the pipe used for callbacks *)
external set_pipe : int -> unit = "set_pipe_c"

(** Allocate memory and register cache directory *)
external alloc_and_set_cache_dir : string -> unit = "alloc_and_set_cache_dir_c"

(** Allocate memory and set up thread for callbacks *)
val init : unit -> Thread.t
