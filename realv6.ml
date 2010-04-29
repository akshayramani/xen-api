let _proprietary_code_marker = "Citrix proprietary code"

module D=Debug.Debugger(struct let name="v6api" end)
open D
open Stringext
open Threadext

type state_type = {
	edition: string;
	licensed: string;
	days_to_expire: int32;
	timestamp: float;
}

let state = ref None

let xd_grace_thread = ref false
let m = Mutex.create ()

let shutdown () =
	debug "shutdown";
	state := None;
	Mutex.execute m (fun () -> xd_grace_thread := false);
	Lpe.stop ()

let initialise address port edition =
	debug "initialise (address = %s, port = %d, edition = %s)"
		address (Int32.to_int port) edition;
		
	(* check edition  *)
	if not (List.mem edition ["STD"; "ADV"; "ENT"; "PLT"; "XD"]) then
		failwith "unknown edition";

	let ts_file = Lpe.v6_cache_dir ^ "/ts" in
	let hash s = string_of_int (String.fold_left (fun t c -> t + (int_of_char c)) 0 s) in
	let write_last_check_data p e =
		try
			let timestamp = string_of_float (Unix.time ()) in
			let days_to_expire = match !state with Some s -> Int32.to_string s.days_to_expire | None -> "-1" in
			let server = address ^ (Int32.to_string port) in
			let data = p ^ "\n" ^ e ^ "\n" ^ timestamp ^ "\n" ^ days_to_expire ^ "\n" ^ server in
			let checksum = hash data in
			let data = data ^ "\n" ^ checksum in
			Unixext.write_string_to_file ts_file data
		with _ -> ()
	in
	let read_last_check_data () =
		try
			(* file should contain last_checkout_time and last_days_to_expire *)
			let data = Unixext.read_whole_file_to_string ts_file in
			let x = String.rindex data '\n' in
			let checksum = String.sub_to_end data (x + 1) in
			let data = String.sub data 0 x in
			debug "%s - %s - %s" data checksum (hash data);
			if hash data <> checksum then
				failwith "someone has tampered with the timestamp file!"
			else
				let data_split = String.split '\n' data in
				match data_split with
				| [a; b; c; d; e] ->
					a, b, float_of_string c, int_of_string d, e
				| _ -> failwith "error reading timestamp file!"
		with _ ->
			"", "", 0., 0, ""
	in
	let init_lpe () =
		if edition = "XD" then
			let last_product, last_edition, last_checkout_time, _, last_server = read_last_check_data () in
			let last_checkout_delta = Unix.time () -. last_checkout_time in
			let days_since_last_checkout = int_of_float (last_checkout_delta /. 3600. /. 24.) in
			let check p e =
				(* The following will not work for XDT licenses, which use the UD profile.
				 * The LPE does not support such licenses... a different solution needs
				 * to be found. *)
				let xd_edition_to_component = ["ENT", "VDE"; "PLT", "VDP"] in
				let started = Lpe.start address (Int32.to_int port) p e Xapi_globs.dbv in
				if started = true then
					let c = List.assoc e xd_edition_to_component in
					let licensed = Lpe.component_licensed c in
					let _ = Lpe.stop () in
					licensed
				else
					Lpe.Unreachable
			in
			let new_check p e =
				(* This function should work with all license types *)
				let result = Lpe.license_check address (Int32.to_int port) p e Xapi_globs.dbv in
				match result with
				| Lpe.Granted_real -> 
					write_last_check_data p e;
					Lpe.Granted_real
				| Lpe.Unreachable ->
					let server = address ^ (Int32.to_string port) in
					if days_since_last_checkout < 30 && last_product = p &&
						last_edition = e && last_server = server then
						Lpe.Granted_grace
					else
						Lpe.Unreachable
				| Lpe.Rejected -> Lpe.Rejected
			in
			let combinations =
				let last = last_product, last_edition in
				let all = ["XDS", "ENT"; "XDS", "PLT"; "XDT", "ENT"; "XDT", "PLT"; "XDS", "STD"; "XDT", "STD"] in
				(* If we recently found a XD license, first try the same again for the
				 * LPE grace functionality to work: move the last hit to the head of 
				 * the queue. *)
				if days_since_last_checkout < 30 && List.mem last all then
					last :: List.filter (fun c -> c <> last) all
				else
					all
			in
			let rec search = function
			| [] -> Lpe.Rejected
			| (p, e) :: tl ->
				(* If the server cannot be reached, stop immediately. Otherwise,
				 * continue trying to find a non-rejection. *)
				let l = new_check p e in
				if l = Lpe.Unreachable then
					Lpe.Unreachable
				else if l = Lpe.Rejected then
					search tl
				else (* real or grace license *)
					l
			in
			let result = search combinations in
			(* the state need not be set here, as the LPE is shut down immediately *)
			match result with
			| Lpe.Granted_real ->
				debug "XD license present on license server";
				let grace_thread () =
					let rec loop () =
						Thread.delay 600.;
						debug "Polling for XD license to reset grace period";
						ignore (search combinations);
						if Mutex.execute m (fun () -> !xd_grace_thread) then
							loop ()
						else
							debug "Stopping XD polling thread"
					in
					loop ()
				in
				Mutex.execute m (fun () ->
					if !xd_grace_thread = false then begin
						xd_grace_thread := true;
						ignore (Thread.create grace_thread ())
					end
				);
				"real", Int32.of_int (-1)
			| Lpe.Granted_grace ->
				let days_to_expire = 30 - days_since_last_checkout in
				ignore(V6alert.send_alert Api_messages.v6_grace_license "The license server is unreachable. However, a grace license is given, as a similar license was successfully checked out recently.");
				"grace", Int32.of_int days_to_expire
			| Lpe.Rejected ->
				debug "XD license NOT present on license server";
				ignore (V6alert.send_alert Api_messages.v6_rejected "The requested license is not available at the license server.");
				"declined", Int32.of_int (-1)
			| Lpe.Unreachable ->
				ignore (V6alert.send_alert Api_messages.v6_comm_error "The license could not be checked out, because the license server could not be reached at the given address/port. Please check the connection details, and verify that the license server is running.");
				"declined", Int32.of_int (-1)
		else (* edition <> "XD" *)
			let v6product = "CXS" in
			if Lpe.start address (Int32.to_int port) v6product edition Xapi_globs.dbv then begin
				let result = Lpe.get_license () in
				match result with
				(* Lpe.checkout_result_t option, Lpe.expiry_t option *)
				| Some Lpe.Granted_real, Some expiry
					->
					debug "Got real license";
					let days_to_expire =
						match expiry with
						| Lpe.Permanent -> debug "Permanent license"; Int32.of_int (-1)
						| Lpe.Days d -> debug "%d days to expire" d; Int32.of_int d
					in
					state := Some {edition = edition;
								   licensed = "real";
								   days_to_expire = days_to_expire;
								   timestamp = Unix.time ()};
					"real", days_to_expire
				| Some Lpe.Granted_grace, _ ->
					let grace_expiry = Lpe.get_grace_expiry v6product in
					begin match grace_expiry with
					| Some hours_left ->
						let days_to_expire = 
							(* round up to avoid getting a 0-day license *)
							if hours_left mod 24 = 0 then
								hours_left / 24
							else
								hours_left / 24 + 1
						in
						debug "Got grace license for %d day(s)" days_to_expire;
						let days_to_expire = Int32.of_int days_to_expire in
						state := Some {edition = edition;
							licensed = "grace";
							days_to_expire = days_to_expire;
							timestamp = Unix.time ()};
						V6alert.send_v6_grace_license ();
						"grace", days_to_expire
					| _ ->
						debug "License declined";
						Lpe.release_license ();
						state := Some {edition = edition;
									   licensed = "declined";
									   days_to_expire = Int32.of_int (-1);
									   timestamp = Unix.time ()};
						V6alert.send_v6_rejected ();
						"declined", Int32.of_int (-1)
					end
				| checkout_result, _ ->
					debug "License declined";
					state := Some {edition = edition;
								   licensed = "declined";
								   days_to_expire = Int32.of_int (-1);
								   timestamp = Unix.time ()};
					begin match checkout_result with
					| Some Lpe.Rejected -> V6alert.send_v6_rejected ()
					| Some Lpe.Unreachable -> V6alert.send_v6_comm_error ()
					| _ -> ()
					end;
					"declined", Int32.of_int (-1)
			end else begin
				V6alert.send_v6_comm_error ();
				"declined", Int32.of_int (-1)
			end
	in	
	match !state with
	| Some s ->
		if s.edition = edition then begin
			debug "Already initialised with same edition; returning state";
			if Int32.to_int s.days_to_expire > -1 then begin
				let days_past = int_of_float ((Unix.time () -. s.timestamp) /. 3600. /. 24.) in
				let days_to_expire = Int32.to_int s.days_to_expire - days_past in
				s.licensed, Int32.of_int days_to_expire
			end else
				s.licensed, s.days_to_expire
		end else begin
			debug "Already initialised, but with different edition; shutting down first";
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
	
