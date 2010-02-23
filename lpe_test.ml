let v6_cache_dir = "lpe-cache"

external set_pipe: int -> unit = "set_pipe_c"
external alloc_and_set_cache_dir: string -> unit = "alloc_and_set_cache_dir_c"

external start: string -> int -> string -> string -> string -> bool = "start_c"
external get_license: string -> bool * bool * int * int = "get_license_c"
external release_license: unit -> bool = "release_license_c"
external stop: unit -> bool = "stop_c"

let init () =
	(* set cache directory for LPE *)
	alloc_and_set_cache_dir v6_cache_dir;

