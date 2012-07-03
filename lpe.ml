let _proprietary_code_marker = "Citrix proprietary code"

module D=Debug.Debugger(struct let name="lpe" end)
open D

(* LPE C functions *)

external set_pipe: int -> unit = "set_pipe_c"
external alloc_and_set_cache_dir: string -> unit = "alloc_and_set_cache_dir_c"

external start_c: string -> int -> string -> string -> string -> bool = "start_c"
external get_license_c: string -> int * int * int = "get_license_c"
external release_license_c: string -> bool = "release_license_c"
external component_status_c: string -> string -> int = "component_status_c"
external stop_c: unit -> bool = "stop_c"
external get_grace_info_c: string -> int = "get_grace_info_c"

external license_check_c: string -> int -> string -> string -> string -> int = "license_check_c"


(* get_license result types *)

type checkout_result_t = Granted_real | Granted_grace | Unreachable | Rejected
type expiry_t = Permanent | Days of int

let string_of_expiry_t = function
	| Permanent -> "Permanent"
	| Days d -> string_of_int d

let expiry_t_of_string = function
	| "Permanent" -> Permanent
	| d -> Days (int_of_string d)

(* Stuff for tracking the server status in the callback thread *)

type server_status_t = Unknown | Up | Down
let m = Mutex.create ()
let c = Condition.create ()
let server_status = ref Unknown

(* The state of the LPE *)

type state_t =
	{
		started: bool;
		address: string;
		port: int;
		product: string;
		edition: string;
		dbv: string;
		profile: string;
		licensed: bool;
	}
let state = ref None	(* None means "not initialised"; init needs to be called *)

let reset_state () =
	state := Some {started = false; address = ""; port = 0; product = "";
		edition = ""; dbv = ""; profile = ""; licensed = false};
	Mutex.lock m;
	server_status := Unknown;
	Mutex.unlock m


(* Starting the callback thread *)

let monitor_callbacks () =
	Debug.with_thread_associated "lpe_callback" (fun () ->
		debug "Started callback-handling thread";
	  let last_alert = ref "" in
	  let pin, pout = Unix.pipe () in
	  set_pipe (Unixext.int_of_file_descr pout);
	  let s = String.create 1 in
	  let rec receive () =
		  try
			  Unixext.really_read pin s 0 1;
			  begin match s with
				  | "u" ->
						Mutex.lock m;
						if !server_status <> Up then
							debug "License server up";
						server_status := Up;
						Condition.signal c;
						Mutex.unlock m;
						last_alert := "u"
				  | "d" ->
						Mutex.lock m;
						if !server_status <> Down then
							debug "License server down";
						server_status := Down;
						Condition.signal c;
						Mutex.unlock m;
						last_alert := "d"
				  | "e" when !last_alert <> "e" ->
						debug "License expired";
				(* ignore(V6alert.send_alert Api_messages.v6_license_expired ""); *)
						last_alert := "e"
				  | "v" ->
						error "Incompatible license-server version!"
				  | x -> debug "Unknown or redundant callback '%s'" x
			  end;
			  receive ()
		  with End_of_file ->
			  error "There was an error in the callback system of the License Policy Engine. Aborted callback thread.";
			raise End_of_file
	  in
	  receive ()
	) ()

(* LPE control functions *)

let init () =
	debug "Initialising";

	(* initialise state *)
	reset_state ();

	(* set cache directory for LPE *)
	Unixext.mkdir_safe V6globs.v6_cache_dir 0o755;
	alloc_and_set_cache_dir V6globs.v6_cache_dir;

	(* set up callback thread *)
	ignore (Thread.create monitor_callbacks ())


let start address port product edition dbv =
	if !state = None then
		init ();

	debug "Starting LPE";
	let result = start_c address port product edition dbv in
	if result = true then
		state := Some {started = true; address = address; port = port; product = product;
			edition = edition; dbv = dbv; profile = ""; licensed = false};
	result


let write_sa_date () =
	let date_str_len = 36 in (* We read 9 chars of 4 bytes each *)
	let read_last_n fn n =
		let fd = Unix.openfile fn [Unix.O_RDONLY] 0 in
		let buff = String.create n in
		begin
			try
				ignore (Unix.lseek fd (-n) Unix.SEEK_END) ;
				let br = Unix.read fd buff 0 n in
				if br < n
				then failwith "Could not read file"
			with exn ->
				Unix.close fd ;
				raise exn
		end ;
		Unix.close fd ;
		buff in
	let string_filter_step str n =
		let out = String.create ((String.length str) / n) in
		let rec loop i j =
			if (j < String.length str) then begin
				String.set out i (String.get str j) ;
				loop (i+1) (j+n)
			end in
		loop 0 0 ;
		out in
	try
		let date_str = read_last_n V6globs.lpe_ini date_str_len in
		Unixext.write_string_to_file
			V6globs.sa_date_filename
			(string_filter_step date_str 4)
	with _ ->
		debug "Caught exception in write_sa_date; SA date not written to file."


let get_license () =
	match !state with
	| Some {licensed = true} ->
		debug "Cannot get license: a license was already checked out";
		None, None
	| Some s when s.started = true ->
		(* The license profile is a special string needed by the GetLicense call of the LPE
		 * It contains the edition and a unique identifier (see LPE API docs on wiki). *)
		let identifier = String.sub (Uuid.to_string (Uuid.make_uuid ())) 0 13 in
		let profile = s.product ^ "_" ^ s.edition ^ "_CCS#" ^ identifier in
		debug "Checking out license; profile: %s" profile;
		let result, expiry, licensed =
			match get_license_c profile with
			| 2, _, _ ->
				(* License server rejected the request *)
				debug "reqStatus = 2: license rejected";
				(* release is needed, as license request is kept in LPE *)
				ignore (release_license_c profile);
				Rejected, None, false
			| reqStatus, _, _ when reqStatus > 2 ->
				(* License server could not be reached *)
				debug "reqStatus = %d: license server unreachable" reqStatus;
				Unreachable, None, false
			| reqStatus, pLicenseGiven, days_to_expire ->
				(* Obtained license *)
				debug "reqStatus = %d, pLicenseGiven = %d, days_to_expire = %d"
					reqStatus pLicenseGiven days_to_expire;
				(* Wait for server status to settle *)
				Mutex.lock m;
				if !server_status = Unknown then
					Condition.wait c m;
				let server = !server_status in
				Mutex.unlock m;
				match server with
				| Up ->
					debug "Checked out a real license";
					let expiry = if days_to_expire = -1 then Permanent else Days days_to_expire in
					Granted_real, Some expiry, true
				| _ -> (* Can only be Down here, not Unknown *)
					debug "Got a grace license";
					Granted_grace, None, true
		in
		debug "Writing SA date to file";
		write_sa_date ();
		state := Some {s with profile = profile; licensed = licensed};
		Some result, expiry
	| _ ->
		debug "Cannot get license: LPE is not running";
		None, None


let release_license () =
	match !state with
	| Some ({licensed = true} as s) ->
		debug "Releasing license; profile: %s" s.profile;
		let result = release_license_c s.profile in
		state := Some {s with profile = ""; licensed = false};
		result
	| _ ->
		debug "No license to release";
		false


let component_licensed component =
	match !state with
	| Some ({started = true} as s) ->
		debug "Checking component status; product: %s, component: %s" s.product component;
		(* Wait for server status to settle *)
		Mutex.lock m;
		if !server_status = Unknown then
			Condition.wait c m;
		let server = !server_status in
		Mutex.unlock m;
		(* Check component status *)
		let success = (component_status_c s.product component) == 0 in
		begin match server, success with
		| Up, true ->
			debug "Component is licensed";
			Granted_real
		| Up, false ->
			debug "Component NOT licensed";
			Rejected
		| Down, true ->
			debug "Component is GRACE licensed";
			Granted_grace
		| Down, false ->
			debug "Communication problem";
			Unreachable
		| _ -> (* Pattern will never match, as server cannot be Unknown here *)
			Unreachable
		end
	| _ ->
		debug "Cannot check status: LPE is not running";
		Unreachable


let stop () =
	match !state with
	| Some {started = true} ->
		ignore (release_license ());
		debug "Stopping LPE";
		let result = stop_c () in
		reset_state ();
		debug "Writing SA date to file";
		write_sa_date ();
		result
	| _ ->
		debug "Cannot stop LPE as it is not running";
		reset_state ();
		false


let license_check address port product edition dbv =
	debug "Checking for %s %s" product edition;
	let result = license_check_c address port product edition dbv in
	match result with
	| 2 -> debug "License present"; Granted_real
	| 1 -> debug "License not present"; Rejected
	| 0 | _ -> debug "Server unreachable"; Unreachable


let get_grace_expiry product =
	match !state with
	| Some {started = true} ->
		debug "Checking grace expiry";
		let hours_left = get_grace_info_c product in
		debug "hours left: %d" hours_left;
		Some hours_left
	| _ ->
		debug "LPE is not running";
		None

