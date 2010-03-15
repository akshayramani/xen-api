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
	Lpe.stop ()

let initialise address port edition =
	debug "initialise (address = %s, port = %d, edition = %s)"
		address (Int32.to_int port) edition;
		
	(* check edition  *)
	if not (List.mem edition ["STD"; "ADV"; "ENT"; "PLT"; "XD"]) then
		failwith "unknown edition";

	let ts_file = Lpe.v6_cache_dir ^ "/ts" in
	let write_last_checkout_data p e =
		let timestamp = string_of_float (Unix.time ()) in
		let days_to_expire = match !state with Some s -> Int32.to_string s.days_to_expire | None -> "-1" in
		Unixext.write_string_to_file ts_file (p ^ "\n" ^ e ^ "\n" ^ timestamp ^ "\n" ^ days_to_expire)
	in
	let read_last_checkout_data () =
		try
			(* file should contain last_checkout_time and last_days_to_expire *)
			let data = Unixext.read_whole_file_to_string ts_file in
			let data_split = String.split '\n' data in
			match data_split with
			| [a; b; c; d] -> a, b, float_of_string c, int_of_string d
			| _ -> failwith "error reading timestamp file"
		with _ ->
			"", "", 0., 0
	in
	let init_lpe () =
		if edition = "XD" then
			let last_product, last_edition, last_checkout_time, _ = read_last_checkout_data () in
			let last_checkout_delta = Unix.time () -. last_checkout_time in
			let days_since_last_checkout = int_of_float (last_checkout_delta /. 3600. /. 24.) in
			let xd_edition_to_component = ["ENT", "VDE"; "PLT", "VDP"] in
			let check p e =
				(* The following will not work for XDT licenses, which use the UD profile.
				 * The LPE does not support such licenses... a different solution needs
				 * to be found. *)
				let started = Lpe.start address (Int32.to_int port) p e Xapi_globs.dbv in
				if started = true then
					let c = List.assoc e xd_edition_to_component in
					let licensed = Lpe.component_licensed c in
					let _ = Lpe.stop () in
					licensed
				else
					Lpe.Unreachable
			in
			let combinations =
				let last = last_product, last_edition in
				let all = ["XDS", "ENT"; "XDS", "PLT"; "XDT", "ENT"; "XDT", "PLT"] in
				(* If we recently found a XD license, first try the same again for the
				 * LPE grace functionality to work: move the last hit to the head of 
				 * the queue. *)
				if days_since_last_checkout < 30 && List.mem last all then
					last :: List.filter (fun c -> c = last) all
				else
					all
			in
			let rec search = function
			| [] -> Lpe.Rejected, "", ""
			| (p, e) :: tl ->
				(* If the server cannot be reached, stop immediately. Otherwise,
				 * continue trying to find a non-rejection. *)
				let l = check p e in
				if l = Lpe.Unreachable then
					Lpe.Unreachable, "", ""
				else if l = Lpe.Rejected then
					search tl
				else (* real or grace license *)
					l, p, e
			in
			let result, v6product, v6edition = search combinations in
			(* the state need not be set here, as the LPE is shut down immediately *)
			match result with
			| Lpe.Granted_real ->
				debug "XD license present on license server";
				write_last_checkout_data v6product v6edition;
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
			let v6product = "CXSTP" in
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
					write_last_checkout_data v6product edition;
					"real", days_to_expire
				| Some Lpe.Granted_grace, _ ->
					(* set grace expiry to 30 days after the last succesful checkout, but
					 * never more than the expiry date of that last checkout *)
					let _, _, last_checkout_time, last_days_to_expire = read_last_checkout_data () in
					if last_checkout_time > 0. then begin
						debug "Got grace license";
						let last_checkout_delta = (Unix.time ()) -. (last_checkout_time) in
						let days_past = int_of_float (last_checkout_delta /. 3600. /. 24.) in
						let max_grace = if last_days_to_expire > -1 then min 30 last_days_to_expire else 30 in
						let days_to_expire = Int32.of_int (max (max_grace - days_past) 0) in
						state := Some {edition = edition;
									   licensed = "grace";
									   days_to_expire = days_to_expire;
									   timestamp = Unix.time ()};
						V6alert.send_v6_grace_license ();
						"grace", days_to_expire
					end else begin
						debug "Signalling a checkout failure";
						V6alert.send_v6_comm_error ();
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
	
