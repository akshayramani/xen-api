module D=Debug.Debugger(struct let name="lpe" end)
open D

let v6_cache_dir = "/var/xapi/lpe-cache"

external initialise: string -> int -> string -> string -> string -> 
	bool * bool * int * int = "initialise_c"
external shutdown: unit -> bool = "shutdown_c"
external set_pipe: int -> unit = "set_pipe_c"
external alloc_and_set_cache_dir: string -> unit = "alloc_and_set_cache_dir_c"

let monitor_callbacks () =
	debug "started callback-handling thread";
	let pin, pout = Unix.pipe () in
	set_pipe (Unixext.int_of_file_descr pout);
	let s = String.create 1 in
	let rec receive () = 
		try
			Unixext.really_read pin s 0 1;
			begin match s with
			| "u" -> debug "license server up"; ignore(V6alert.send_alert Api_messages.v6_server_up "")
			| "d" -> debug "license server down"; ignore(V6alert.send_alert Api_messages.v6_server_down "")
			| "e" -> debug "license expired"; ignore(V6alert.send_alert Api_messages.v6_license_expired "")
			| x -> debug "unknown callback option '%s'" x
			end;
			receive ()
		with End_of_file ->
			error "There was an error in the callback system of the License Policy Engine. Aborted callback thread.";
			raise End_of_file
	in
	receive ()

let init () =
	(* set cache directory for LPE *)
	Unixext.mkdir_safe v6_cache_dir 0o755;
	alloc_and_set_cache_dir v6_cache_dir;
	
	(* set up callback thread *)
	Thread.create monitor_callbacks ()

