external set_pipe: int -> unit = "set_pipe_c"
external alloc_and_set_cache_dir: string -> unit = "alloc_and_set_cache_dir_c"

external start_c: string -> int -> string -> string -> string -> bool = "start_c"
external get_license_c: string -> int * int * int = "get_license_c"
external release_license_c: string -> bool = "release_license_c"
external component_status_c: string -> string -> int = "component_status_c"
external stop_c: unit -> bool = "stop_c"
external get_grace_info_c: string -> int = "get_grace_info_c"

external license_check_c: string -> int -> string -> string -> string -> int = "license_check_c"

let init () =
	(* set cache directory for LPE *)
	alloc_and_set_cache_dir "lpe-cache"

