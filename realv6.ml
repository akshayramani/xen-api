module Make(E : module type of Edition)
           (Lpe : module type of Lpe) = (struct

let _proprietary_code_marker = "Citrix proprietary code"

module D=Debug.Debugger(struct let name="v6api" end)
open D
open Stringext
open Threadext

module L  = License.Make(E)
module LF = License_file.Make(E)

type state_type = {
	edition: string;
	licensed: string;
	days_to_expire: Lpe.expiry_t;
	timestamp: float;
}

(* define "never" as 01-01-2030 *)
let never, _ =
	let start_of_epoch = Unix.gmtime 0. in
	Unix.mktime {start_of_epoch with Unix.tm_year = 130}

let state = ref None

let xd_grace_thread = ref false
let m = Mutex.create ()

let shutdown () =
	debug "shutdown";
	state := None;
	Mutex.execute m (fun () -> xd_grace_thread := false);
	Lpe.stop ()

let initialise address port edition sockets =
	debug "initialise (address = %s, port = %d, edition = %s, sockets = %d)"
		address (Int32.to_int port) edition sockets;

	(* check edition  *)
	if not (List.mem edition ["SKT"; "STD"; "ADV"; "ENT"; "PLT"; "XD"]) then
		failwith "unknown edition";

	let ts_file = V6globs.v6_cache_dir ^ "/ts" in
	let hash s = string_of_int (String.fold_left (fun t c -> t + (int_of_char c)) 0 s) in
	let write_last_check_data p e =
		try
			let timestamp = string_of_float (Unix.time ()) in
			let days_to_expire = match !state with
				| Some s -> Lpe.string_of_expiry_t s.days_to_expire
				| None -> Lpe.string_of_expiry_t Lpe.Permanent in
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
			let data = Unixext.string_of_file ts_file in
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
					a, b, float_of_string c, Lpe.expiry_t_of_string d, e
				| _ -> failwith "error reading timestamp file!"
		with _ ->
			"", "", 0., Lpe.Days 0, ""
	in

	let init_lpe () =
		if edition = "XD" then
			let last_product, last_edition, last_checkout_time, _, last_server = read_last_check_data () in
			let last_checkout_delta = Unix.time () -. last_checkout_time in
			let days_since_last_checkout = int_of_float (last_checkout_delta /. 3600. /. 24.) in
			(** This is a better way to check licenses, but since the
				LPE doesn't fully support these licenses, we have to
				resort to the new_check below. Commenting this out because
				we want to revert to this one day, but leaving it in
				causes a compiler warning, and we have warn-errors
				enabled.  *)
			(* let check p e = *)
			(* 	(\* The following will not work for XDT licenses, which use the UD profile. *)
			(* 	 * The LPE does not support such licenses... a different solution needs *)
			(* 	 * to be found. *\) *)
			(* 	let xd_edition_to_component = ["ENT", "VDE"; "PLT", "VDP"] in *)
			(* 	let started = Lpe.start address (Int32.to_int port) p e V6globs.dbv in *)
			(* 	if started = true then *)
			(* 		let c = List.assoc e xd_edition_to_component in *)
			(* 		let licensed = Lpe.component_licensed c in *)
			(* 		let _ = Lpe.stop () in *)
			(* 		licensed *)
			(* 	else *)
			(* 		Lpe.Unreachable *)
			(* in *)

			let new_check p e =
				(* This function should work with all license types *)
				let result = Lpe.license_check address (Int32.to_int port) p e V6globs.dbv in
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
				(* this case is impossible... *)
				| Lpe.Granted_grace ->
					write_last_check_data p e;
					Lpe.Granted_grace
			in
			let combinations =
				let last = last_product, last_edition in
				let all = ["XDS", "ENT"; "XDS", "PLT"; "XDT", "ENT"; "XDT", "PLT"; "XDS", "STD"; "XDT", "STD"; "CDIAB" , "ADV"] in
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
				"real", Lpe.Permanent
			| Lpe.Granted_grace ->
				let days_to_expire = 30 - days_since_last_checkout in
				ignore(V6alert.send_alert Api_messages.v6_grace_license "The license server is unreachable. However, a grace license is given, as a similar license was successfully checked out recently.");
				"grace", Lpe.Days days_to_expire
			| Lpe.Rejected ->
				debug "XD license NOT present on license server";
				ignore (V6alert.send_alert Api_messages.v6_rejected "The requested license is not available at the license server.");
				"declined", Lpe.Permanent
			| Lpe.Unreachable ->
				ignore (V6alert.send_alert Api_messages.v6_comm_error "The license could not be checked out, because the license server could not be reached at the given address/port. Please check the connection details, and verify that the license server is running.");
				"declined", Lpe.Permanent

		else (* edition <> "XD" *)
			if Lpe.start address (Int32.to_int port) V6globs.v6product edition V6globs.dbv sockets then begin
				let result = Lpe.get_license () in
				match result with
				(* Lpe.checkout_result_t option, Lpe.expiry_t option *)
				| Some Lpe.Granted_real, Some expiry ->
					debug "Got real license";
					(match expiry with
						| Lpe.Permanent -> debug "Permanent license"
						| Lpe.Days d -> debug "%d days to expire" d);
					state := Some {edition = edition;
								   licensed = "real";
								   days_to_expire = expiry;
								   timestamp = Unix.time ()};
					"real", expiry
				| Some Lpe.Granted_grace, _ ->
					let grace_expiry = Lpe.get_grace_expiry V6globs.v6product in
					begin match grace_expiry with
					| Some hours_left ->
						let days_to_expire = hours_left / 24 in
						debug "Got grace license for %d day(s)" days_to_expire;
						state := Some {edition = edition;
							licensed = "grace";
							days_to_expire = Lpe.Days days_to_expire;
							timestamp = Unix.time ()};
						V6alert.send_v6_grace_license ();
						"grace", Lpe.Days days_to_expire
					| _ ->
						debug "License declined";
						ignore (Lpe.release_license ());
						state := Some {edition = edition;
									   licensed = "declined";
									   days_to_expire = Lpe.Permanent;
									   timestamp = Unix.time ()};
						V6alert.send_v6_rejected ();
						"declined", Lpe.Permanent
					end
				| checkout_result, _ ->
					debug "License declined";
					state := Some {edition = edition;
								   licensed = "declined";
								   days_to_expire = Lpe.Permanent;
								   timestamp = Unix.time ()};
					begin match checkout_result with
					| Some Lpe.Rejected -> V6alert.send_v6_rejected ()
					| Some Lpe.Unreachable -> V6alert.send_v6_comm_error ()
					| _ -> ()
					end;
					"declined", Lpe.Permanent
			end else begin
				V6alert.send_v6_comm_error ();
				"declined", Lpe.Permanent
			end
	in

	match !state with
	| Some _ ->
		debug "Already initialised; shutting down first to release current licenses";
		ignore (shutdown ());
		init_lpe ()
	| None -> init_lpe ()


let supported_editions = [E.Free; E.Socket; E.XenDesktop]

let v6edition = function
	| E.Socket -> "STD"
	| E.XenDesktop -> "XD"
	| E.Free -> ""

let write_grace_to_file grace_expiry =
	let grace_expiry_str = string_of_float grace_expiry in
	Unixext.write_string_to_file Xapi_globs.upgrade_grace_file grace_expiry_str

let read_grace_from_file () =
	try
		let grace_expiry_str = Unixext.string_of_file Xapi_globs.upgrade_grace_file in
		float_of_string grace_expiry_str
	with _ -> 0.

let apply_edition dbg edition additional = Debug.with_thread_associated dbg (fun () ->
	(* default is free edition *)
	let default_license = L.default () in
	let current_edition = List.assoc "current_edition" additional in
	let startup = List.mem_assoc "startup" additional && List.assoc "startup" additional = "true" in
	let sockets = if List.mem_assoc "sockets" additional
		then List.assoc "sockets" additional else "" in
	let sockets = try int_of_string sockets with _ ->
		debug "Couldn't read number of sockets %s, defaulting to 1" sockets ; 1 in
	let free_license = {
		default_license with L.grace="no";
		L.expiry=never;
		L.sockets = sockets;
	} in

	let get_license edition current_license =
		try
			Reapply.cancel (); (* cancel any existing re-apply timer *)
			let edition' = E.of_string edition in
			if not (List.mem edition' supported_editions) then
				raise (E.Undefined_edition edition);
			let current_edition =
				if current_edition = "" then
					E.to_string E.Free
				else
					current_edition
			in
			match edition' with
			| E.Free ->
				if startup
				then begin
					debug "Applying free edition" ;
					free_license
				end
				else begin
					debug "Releasing license and applying free edition" ;
					ignore (shutdown ()) ;
					free_license
				end

			| e ->
				(* Try to get the a v6 license; if one has already been checked out,
				 * it will be automatically checked back in. *)
				if E.of_string current_edition = E.Free then
					info "Upgrading from 'free' to '%s' edition..." edition
				else
					info "(Re)applying %s license..." edition;

				let address, port =
					try
						List.assoc "address" additional,
						List.assoc "port" additional
					with Not_found ->
						raise (V6errors.Error (V6errors.missing_connection_details, []))
				in
				let license, days_to_expire = initialise address (Int32.of_string port) (v6edition e) sockets in

				(* set expiry date *)
				let now = Unix.time () in
				let expires =
					let day = 24. *. 60. *. 60. in
					(* Round to the nearest day, then bump day forward. *)
					let next_midnight = (floor (now /. day)) *. day +. day in
					match days_to_expire with
						| Lpe.Days d -> next_midnight +. (float_of_int d *. day)
					| Lpe.Permanent -> never
				in

				(* check fist point *)
				let expires =
					(* CA-33155: FIST point may only set an expiry date earlier than the actual one *)
					begin match V6fist.set_expiry_date () with
						| None -> expires
						| Some d ->
							let fist_date = Date.to_float (Date.of_string d) in
							if fist_date < expires then fist_date else expires
					end
				in

				let upgrade_grace = read_grace_from_file () > Unix.time () in
				if license = "real" || license = "grace" then begin
					info "Checked out %s %s license from license server." edition license;
					(* delete upgrade-grace file, if it exists *)
					Unixext.unlink_safe Xapi_globs.upgrade_grace_file;
					let name = E.to_marketing_name edition' in
					if license = "grace" then begin
						let grace_retry_period =
							if V6fist.reduce_grace_retry_period () then
								V6globs.reduced_grace_retry_period
							else
								V6globs.grace_retry_period
						in
						Reapply.start edition grace_retry_period;

						let expires =
							if V6fist.reduce_grace_period () then
								now +. (15. *. 60.)
							else
								expires
						in
						{
							default_license with
							L.sku = edition;
							L.sku_marketing_name = name;
							L.sockets = sockets;
							L.expiry = expires;
							L.grace = "regular grace";
						}
					end else begin
						let reapply_period =
							match V6fist.set_reapply_period () with
							| None -> V6globs.reapply_period
							| Some period ->
								try
									float_of_string (String.strip String.isspace period)
								with _ ->
									debug "Failed to interpret re-apply period from FIST point: \"%s\"" period;
									V6globs.reapply_period
						in
						Reapply.start edition reapply_period;

						{
							default_license with
							L.sku = edition;
							L.sku_marketing_name = name;
							L.sockets = sockets;
							L.expiry = expires
						}
					end
				end else if edition = current_edition && upgrade_grace then begin
					info "No %s license is available, but we are still in the upgrade grace period." current_edition;
					{current_license with L.grace = "upgrade grace"}
				end else if V6globs.beta or (List.mem_assoc "earlyrelease" additional) then begin
					info "Upgrade from beta: transition to GA (30-day grace license).";
					let expiry = L.upgrade_grace_expiry () in
					write_grace_to_file expiry;
					V6alert.send_v6_upgrade_grace_license ();
					let name = E.to_marketing_name (E.of_string edition) in
					{default_license with
						L.sku = current_edition;
						L.sku_marketing_name = name;
						L.expiry = expiry;
						L.grace = "upgrade grace"}
				end else if startup then begin
					info "No '%s' license is available. Returning expired license." current_edition;
					(* expiry date 0 means 01-01-1970, so always expired *)
					{current_license with L.expiry = 0.}
				end else begin
					error "License could not be checked out. Edition is not changed.";
					ignore (shutdown ());
					raise (V6errors.Error (V6errors.license_checkout_error, [edition]))
				end
		with E.Undefined_edition e ->
			raise (V6errors.Error (V6errors.invalid_edition, [edition]))
	in
	let effective_edition, new_license =
		(* If edition is blank, use Free as the default. This indicates startup after a fresh install,
		 * or the application of a new license file. *)
		let edition = if edition = "" then E.to_string E.Free else edition in
		try
			let current_license = L.of_assoc_list additional in
			let new_license = get_license edition current_license in
			let effective_edition = if new_license.L.expiry = 0.
				then E.to_string E.Free
				else new_license.L.sku in
			effective_edition, new_license
		with L.Missing_license_param _ ->
			(* No current license params: first boot -> give a default license.
			 * If an activation key exists, this will used. *)
			edition,
			get_license edition default_license
	in
	new_license.L.sku, E.to_features (E.of_string new_license.L.sku),
		(L.to_assoc_list new_license) @ V6globs.early_release @
		(Additional_features.to_assoc_list (E.to_additional_features (E.of_string effective_edition)))
	) () (* Debug.with_thread_associated *)

let get_editions dbg =
	Debug.with_thread_associated dbg (fun () ->
		List.map (fun e -> E.to_string e, E.to_marketing_name e,
			E.to_short_string e, E.to_int e) supported_editions
	) ()

let get_version dbg =
	Debug.with_thread_associated dbg (fun () ->
		V6globs.dbv
	) ()

let reopen_logs () = true

end : V6rpc.V6api)
