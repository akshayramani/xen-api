let _proprietary_code_marker = "Citrix proprietary code"

module D=Debug.Debugger(struct let name="lpe" end)
open D

(* Directory in which the LPE stores its cache *)
let v6_cache_dir = "/var/xapi/lpe-cache"

(* LPE C functions *)

external set_pipe: int -> unit = "set_pipe_c"
external alloc_and_set_cache_dir: string -> unit = "alloc_and_set_cache_dir_c"

external start_c: string -> int -> string -> string -> string -> bool = "start_c"
external get_license_c: string -> int * int * int = "get_license_c"
external release_license_c: string -> bool = "release_license_c"
external component_status_c: string -> string -> int = "component_status_c"
external stop_c: unit -> bool = "stop_c"

external license_check_c: string -> int -> string -> string -> string -> int = "license_check_c"

(* get_license result types *)

type checkout_result_t = Granted_real | Granted_grace | Unreachable | Rejected
type expiry_t = Permanent | Days of int

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
				debug "License server up";
				Mutex.lock m;
				server_status := Up;
				Condition.signal c;
				Mutex.unlock m;
				(*if !last_alert <> "u" then ignore(V6alert.send_alert Api_messages.v6_server_up ""); *)
				last_alert := "u"
			| "d" ->
				debug "License server down";
				Mutex.lock m;
				server_status := Down;
				Condition.signal c;
				Mutex.unlock m;
				(* if !last_alert <> "d" then ignore(V6alert.send_alert Api_messages.v6_server_down ""); *)
				last_alert := "d"
			| "e" when !last_alert <> "e" ->
				debug "License expired";
				ignore(V6alert.send_alert Api_messages.v6_license_expired "");
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


(* LPE control functions *)

let init () =
	debug "Initialising";

	(* initialise state *)
	reset_state ();
	
	(* set cache directory for LPE *)
	Unixext.mkdir_safe v6_cache_dir 0o755;
	alloc_and_set_cache_dir v6_cache_dir;
	
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
		result
	| _ ->
		debug "Cannot stop LPE as it is not running";
		reset_state ();
		false
		

let license_check address port product edition dbv =
	let result = license_check_c address port product edition dbv in
	match result with
	| 2 -> Granted_real
	| 1 -> Rejected
	| 0 | _ -> Unreachable
	
