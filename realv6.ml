let _proprietary_code_marker = "Citrix proprietary code"

module D=Debug.Debugger(struct let name="v6api" end)
open D
open Stringext

type state_type = {
	edition: string;
	licensed: string;
	days_to_expire: int32;
	timestamp: float;
}

let state = ref None

let shutdown () =
	debug "shutdown";
	state := None;
	Lpe.shutdown ()

let initialise address port edition =
	debug "initialise (address = %s, port = %d, edition = %s)"
		address (Int32.to_int port) edition;
		
	(* check edition  *)
	if not (List.mem edition ["STD"; "ADV"; "ENT"; "PLT"]) then
		failwith "unknown edition";

	let ts_file = Lpe.v6_cache_dir ^ "/ts" in
	let write_last_checkout_data () =
		let timestamp = string_of_float (Unix.time ()) in
		let days_to_expire = match !state with Some s -> Int32.to_string s.days_to_expire | None -> "-1" in
		Unixext.write_string_to_file ts_file (timestamp ^ "\n" ^ days_to_expire)
	in
	let read_last_checkout_data () =
		try
			let data = Unixext.read_whole_file_to_string ts_file in
			let data_split = String.split '\n' data in
			match data_split with
			| [a; b] -> float_of_string a, int_of_string b
			| _ -> failwith "error reading timestamp file"
		with _ -> 0., 0
	in
	let init_lpe () =
		(* The license profile is a special string needed by the GetLicense call of the LPE
		 * It contains the edition and a unique identifier (see LPE API docs on wiki). *)
		let identifier = String.sub (Uuid.to_string (Uuid.make_uuid ())) 0 13 in
		let license_profile = "CXS_" ^ edition ^ "_CCS#" ^ identifier in
		let result = Lpe.initialise address (Int32.to_int port) edition license_profile Xapi_globs.dbv in
		match result with
		(* licensed, grace, days_to_expire, status code *)
		| true, false, days_to_expire, _ ->
			debug "got real license, %d days to expire" days_to_expire;
			state := Some {edition = edition;
						   licensed = "real";
						   days_to_expire = Int32.of_int days_to_expire;
						   timestamp = Unix.time ()};
			write_last_checkout_data ();
			"real", Int32.of_int days_to_expire
		| true, true, _, _ ->
			debug "got grace license";
			(* set grace expiry to 30 days after the last succesful checkout, but
			 * never more than the expiry date of that last checkout *)
			let last_checkout_time, last_days_to_expire = read_last_checkout_data () in
			let last_checkout_delta = (Unix.time ()) -. (last_checkout_time) in
			let days_past = int_of_float (last_checkout_delta /. 3600. /. 24.) in
			let days_to_expire = max ((min 30 last_days_to_expire) - days_past) 0 in
			state := Some {edition = edition;
						   licensed = "grace";
						   days_to_expire = Int32.of_int days_to_expire;
						   timestamp = Unix.time ()};
			ignore(V6alert.send_alert Api_messages.v6_grace_license "The license server is unreachable. However, a grace license is given, as a similar license was successfully checked out recently.");
			"grace", Int32.of_int days_to_expire
		| false, _, days_to_expire, status ->
			debug "license declined, checkout status: %d" status;
			state := Some {edition = edition;
						   licensed = "declined";
						   days_to_expire = Int32.of_int days_to_expire;
						   timestamp = Unix.time ()};
			begin match status with
			| 2 -> ignore (V6alert.send_alert Api_messages.v6_rejected "The requested license is not available at the license server.")
			| _ -> ignore (V6alert.send_alert Api_messages.v6_comm_error "The license could not be checked out, because the license server could not be reached at the given address/port. Please check the connection details, and verify that the license server is running.")
			end;
			"declined", Int32.of_int days_to_expire
	in	
	match !state with
	| Some s ->
		if s.edition = edition then begin
			debug "already initialised with same edition; returning state";
			if Int32.to_int s.days_to_expire > -1 then begin
				let days_past = int_of_float ((Unix.time () -. s.timestamp) /. 3600. /. 24.) in
				let days_to_expire = Int32.to_int s.days_to_expire - days_past in
				s.licensed, Int32.of_int days_to_expire
			end else
				s.licensed, s.days_to_expire
		end else begin
			debug "already initialised, but with different edition; shutting down first";
			shutdown ();
			init_lpe ()
		end
	| None -> init_lpe ()

let reopen_logs () =
	try
		debug "Reopening logfiles";
		Logs.reopen ();
		debug "Logfiles reopened";
		true
	with _ -> false
	
