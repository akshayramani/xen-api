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
				let started = Lpe.start address (Int32.to_int port) p e V6globs.dbv in
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
			if Lpe.start address (Int32.to_int port) V6globs.v6product edition V6globs.dbv then begin
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
					let grace_expiry = Lpe.get_grace_expiry V6globs.v6product in
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
		if s.edition = edition && s.licensed = "real" then begin
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


open Edition

let supported_editions = [Free; Advanced; Enterprise; Enterprise_xd; Platinum]
let v6edition = function
	| Edition.Advanced -> "ADV"
	| Edition.Enterprise -> "ENT"
	| Edition.Enterprise_xd -> "XD"
	| Edition.Platinum -> "PLT"
	| Edition.Free -> ""

let write_grace_to_file grace_expiry =
	let grace_expiry_str = string_of_float grace_expiry in
	Unixext.write_string_to_file Xapi_globs.upgrade_grace_file grace_expiry_str

let read_grace_from_file () =
	try
		let grace_expiry_str = Unixext.string_of_file Xapi_globs.upgrade_grace_file in
		float_of_string grace_expiry_str
	with _ -> 0.

let apply_edition edition additional =
	(* default is free edition with 30 day grace validity *)
	let default_license = License.default () in
	let current_edition = List.assoc "current_edition" additional in
	let startup = List.mem_assoc "startup" additional && List.assoc "startup" additional = "true" in
	let get_license edition current_license =
		try
			Grace_retry.cancel (); (* cancel any existing grace-retry timer *)
			let edition' = Edition.of_string edition in
			if not (List.mem edition' supported_editions) then
				raise (Edition.Undefined_edition edition);
			let current_edition =
				if current_edition = "" then
					Edition.to_string Edition.Free
				else
					current_edition
			in
			match edition' with
			| Edition.Free ->
				if startup || List.mem_assoc "license_file" additional then begin
					info "Attempting to apply 'free' edition activation key.";
					let license_file =
						if List.mem_assoc "license_file" additional then
							List.assoc "license_file" additional
						else
							!License_file.filename
					in
					debug "License file: %s" !License_file.filename;
					begin try
						let new_license = License_file.do_parse_and_validate license_file in
						if license_file <> !License_file.filename then
							Unix.rename license_file !License_file.filename;
						info "Holding 'free' edition license with expiry date %s."
							(Date.to_string (Date.of_float new_license.License.expiry));
						new_license
					with
					| License_file.License_expired l when startup -> l (* keep expired license *)
					| _ when startup ->
						(* activation file does not exist or is invalid *)
						if current_license.License.expiry < default_license.License.expiry then begin
							info "Existing 'free' license with expiry date %s still in effect."
								(Date.to_string (Date.of_float current_license.License.expiry));
							{
								default_license with
								License.expiry = current_license.License.expiry
							}
						end else begin
							info "Generating 'free' edition grace license, which needs to be activated in 30 days.";
							default_license
						end
					| License_file.License_expired l ->
						raise (V6errors.Error(V6errors.license_expired, []))
					| License_file.License_file_deprecated ->
						raise (V6errors.Error(V6errors.license_file_deprecated, []))
					| e ->
						begin
							debug "Exception processing license: %s" (Printexc.to_string e);
							raise (V6errors.Error(V6errors.license_processing_error, []))
						end
					end
				end else if Edition.of_string current_edition = Edition.Free then begin
					info "The host's edition is already 'free', and not applying a new activation key or starting xapi. No change.";
					current_license
				end else begin
					info "Downgrading from '%s' to 'free' edition." current_edition;
					shutdown ();
					(* delete activation key, if it exists *)
					Unixext.unlink_safe !License_file.filename;
					default_license
				end
			| e ->
				(* Ensure we are not trying to apply an old-style license file here. *)
				if List.mem_assoc "license_file" additional then
					raise (V6errors.Error (V6errors.activation_while_not_free, []));

				(* Try to get the a v6 license; if one has already been checked out,
				 * it will be automatically checked back in. *)
				if Edition.of_string current_edition = Edition.Free then
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
				let license, days_to_expire = initialise address (Int32.of_string port) (v6edition e) in

				(* define "never" as 01-01-2030 *)
				let start_of_epoch = Unix.gmtime 0. in
				let never, _ = Unix.mktime {start_of_epoch with Unix.tm_year = 130} in
	
				(* set expiry date *)
				let now = Unix.time () in
				let expires =
					if days_to_expire > -1l then
						now +. (Int32.to_float days_to_expire *. 24. *. 3600.)
					else
						never
				in
				(* check fist point *)
				let expires =
					(* CA-33155: FIST point may only set an expiry date earlier than the actual one *)
					begin match Xapi_fist.set_expiry_date () with
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
					let name = Edition.to_marketing_name edition' in
					let basic =
						{
							default_license with
							License.sku = edition;
							License.sku_marketing_name = name;
							License.expiry = expires
						}
					in
					if license = "grace" then begin
						Grace_retry.start edition;
						{
							basic with
							License.grace = "regular grace"
						}
					end else
						basic
				end else if edition = current_edition && upgrade_grace then begin
					info "No %s license is available, but we are still in the upgrade grace period." current_edition;
					{current_license with License.grace = "upgrade grace"}
				end else if startup then begin
					info "No %s license is available. License is set to 'expired' (no VMs can start)." current_edition;
					(* expiry date 0 means 01-01-1970, so always expired *)
					{current_license with License.expiry = 0.}
				end else begin
					error "License could not be checked out. Edition is not changed.";
					shutdown ();
					raise (V6errors.Error (V6errors.license_checkout_error, [edition]))
				end
		with Edition.Undefined_edition e ->
			raise (V6errors.Error (V6errors.invalid_edition, [edition]))
	in
	let new_edition, new_license =
		(* If edition is blank, use Free as the default. This indicates startup after a fresh install,
		 * or the application of a new license file. *)
		let edition = if edition = "" then Edition.to_string Edition.Free else edition in
		try
			let current_license = License.of_assoc_list additional in
			edition,
			get_license edition current_license
		with License.Missing_license_param _ ->
			(* No current license params: first boot -> give a default license.
			 * If an activation key exists, this will used. *)
			edition,
			get_license edition default_license
	in
	new_edition, Edition.to_features (Edition.of_string new_edition),
		(License.to_assoc_list new_license) @ V6globs.early_release @
		(Additional_features.to_assoc_list (Edition.to_additional_features (Edition.of_string new_edition)))
		
let get_editions () =
	List.map (fun e -> Edition.to_string e, Edition.to_marketing_name e,
		Edition.to_short_string e, Edition.to_int e) supported_editions

let get_version () =
	V6globs.dbv

let reopen_logs () =
	try
		debug "Reopening logfiles";
		Logs.reopen ();
		debug "Logfiles reopened";
		true
	with _ -> false
	
