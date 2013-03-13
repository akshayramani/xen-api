let _proprietary_code_marker = "Citrix proprietary code"

(* Mock license file structure:
	<file> := <line>*
	<line> := <prod> <quantity> <expiration> <sa_date> <newline>
	        | <prod> <edition> <quantity> <expiration> <sa_date> <newline>
	        | <comment> <newline>
	<prod> := "XD" | "XS"
	<edition> := "SKT" | "ADV" | "ENT" | "PLT"
	<quantity> := <positive integer>
	<expiration> := <float> (unix timestamp)
	<sa_date> := <float> (unix timestamp)
	<newline> := "\n"
	<comment> := <whitespace>* "#" <any string> *)

module D=Debug.Debugger(struct let name="lpe_mock_stubs" end)
open D

let (|>) a f = f a

let never = 1893456000. (* 1 Jan 2030 timestamp *)

type state_t =
	{ started: bool
	; address: string
	; port: int
	; product: string
	; edition: string
	; dbv: string
	; profile: string
	; licensed: bool }

type license_t =
	{ prod : [ `XD | `XS ]
	; ed : [ `SKT | `ADV | `ENT | `PLT ] option
	; quantity : int ref
	; expiry : float
	; sa_date : string }

let state = ref None
let licenses = ref ([] : license_t list)
let licenses_checked_out = (Hashtbl.create 10 : (string, license_t) Hashtbl.t)

let days_of_seconds s = s /. 86400. |> int_of_float (* seconds in day *)

(* Look for mock license files in this order *)
let mock_license_files = [ "mock.lic"; "/tmp/mock.lic"]
let mock_state_files = [ "mock.state"; "/tmp/mock.state" ]

(* State file helper functions *)

let rec open_file = function
	| [] -> None
	| f :: fs ->
		let fi = try Some (open_in f) with _ -> None
		in match fi with
		| None -> open_file fs
		| Some _ -> fi

let read_lines fi =
	let rec loop acc =
		let l = try Some (input_line fi) with _ -> None
		in match l with
			| None -> List.rev acc
			| Some l -> loop (l :: acc)
	in loop []

let read_first_file files =
	let fi = open_file files in
	match fi with
	| None -> []
	| Some fi ->
		let lines = read_lines fi in
		close_in fi ;
		lines

(* WARNING! Str not thread safe! Switch to re library ASAP! *)
let split = Str.(split (regexp "[ \t]+"))
let contains str sub = Str.(string_match (regexp (".*" ^ sub ^ ".*")) str 0)

let prod_of_string p =
	if contains p "XS" then `XS else
	if contains p "XD" then `XD else
	failwith ("Unknown product " ^ p)

let edition_of_string = function
	| "SKT" -> `SKT
	| "ADV" -> `ADV
	| "ENT" -> `ENT
	| "PLT" -> `PLT
	| e -> failwith ("Unknown edition " ^ e)

let license_of_string s =
	let ss = split s in
	match ss with
	| comment :: _ when s.[0] = '#' -> None
	| [p; q; x; s] as line ->
		(try
			Some { prod = prod_of_string p
				 ; ed = None
			     ; quantity = ref (int_of_string q)
			     ; expiry = float_of_string x
			     ; sa_date = s }
		with _ ->
			debug "Bad mock license: %s" (String.concat " " line) ; None)
	| [p; e; q; x; s] as line ->
		(try
			Some { prod = prod_of_string p
			     ; ed = Some (edition_of_string e)
			     ; quantity = ref (int_of_string q)
			     ; expiry = float_of_string x
			     ; sa_date = s }
		with _ ->
			debug "Bad mock license: %s" (String.concat " " line) ; None)
	| line ->
		debug "Bad mock license: %s" s ;
		None

let read_mock_license files =
	read_first_file files
		|> List.map license_of_string
		|> List.filter ((<>) None)
		|> List.map (function | Some x -> x | _ -> failwith "Unpossible!")

let state_of_string s =
	let ss = split s in
	match ss with
	| [up; address; port] ->
		(try
			let up = bool_of_string up
			and port = int_of_string port in
			Some (up, address, port)
		with _ -> None)
	| _ -> None

let read_state_file files =
	read_first_file files
		|> List.map state_of_string
		|> List.filter ((<>) None)
		|> fun ss -> try List.hd ss with _ -> None

let days_until_expiry expiry =
	let seconds_until_expiry = expiry -. Unix.time () in
	if expiry >= never
	then -1
	else days_of_seconds seconds_until_expiry

let is_server_up () =
	match read_state_file mock_state_files with
	| None -> true
	| Some (up, _, _) -> up

let string_of_prod = function
	| `XD -> "XD" | `XS -> "XS"

let split_profile profile =
	debug "Splitting profile '%s'" profile ;
	let i_prod = String.index profile '_' in
	let i_edit = String.index_from profile i_prod '_' in
	let product = String.sub profile 0 i_prod
	and edition = String.sub profile (i_prod+1) i_edit
	in
	debug "product: %s, edition: %s" product edition ;
	(prod_of_string product, edition_of_string edition)

let reset_state () =
	state := Some {started = false; address = "localhost"; port = 27000; product = "";
	               edition = ""; dbv = ""; profile = ""; licensed = false}

let callback_fd = ref None

let set_pipe i =
	debug "Setting callback pipe to %d" i ;
	callback_fd := Some (Unixext.file_descr_of_int i)

let alloc_and_set_cache_dir d =
	debug "Prentending to set cache dir to %s" d

let write_to_callback_pipe msg =
	if msg <> "" then
	match !callback_fd with
	| None -> failwith "Callback thread not set"
	| Some fd -> Unixext.really_write fd msg 0 1

let start_c address port product edition dbv =
	debug "Starting mock LPE engine" ;
	licenses := read_mock_license mock_license_files ;
	state := Some {started = true; address = address; port = port; product = product;
	               edition = edition; dbv = dbv; profile = ""; licensed = false};
	(* XXX if address,port are different than mock.state, we should return "d". *)
	write_to_callback_pipe "u" ;
	true

let stop_c () = state := None ; true

let checkout_license state profile =
	(* Get the license that matches product, that expires last, or None *)
	let product, edition = split_profile profile in
	let license = List.(!licenses
		|> filter (fun l ->
			debug "XXX l.quantity=%d; l.prod=%s; profile=%s"
				!(l.quantity) (string_of_prod l.prod) profile ;
			!(l.quantity) > 0 && l.prod = product && l.ed = Some edition)
		|> sort (fun a b -> compare b.expiry a.expiry)
		|> function | hd :: _ -> Some hd | [] -> None)
	in
	let server_up = is_server_up () in
	debug "XXX found a license? %b. is server up? %b" (license <> None) server_up;
	match server_up, license with
	| false, _ ->
		if state.licensed
		then `granted_grace
		else `unreachable
	| true, None -> `rejected
	| true, Some l -> begin
		if Hashtbl.mem licenses_checked_out profile
		then begin
			debug "Already checked out mock license for profile %s" profile ;
			`rejected
		end
		else begin
			decr l.quantity ;
			Hashtbl.add licenses_checked_out profile l ;
			`granted_real (days_until_expiry l.expiry)
		end
	end

let get_license_c profile =
	info "Attempting to get mock license" ;
	let comm_failure = 3, 0, 0
	and grace_license = 0, 3, 0
	and real_license days = 0, 2, days
	and rejected = 2, 0, 0
	in
	match !state with
	| None -> comm_failure
	| Some s ->
		let result = checkout_license s profile in
		let return, licensed =
			match result with
			| `rejected -> rejected, false
			| `unreachable -> comm_failure, false
			| `granted_grace -> grace_license, true
			| `granted_real d -> real_license d, true
		in
		state := Some {s with profile = profile; licensed = licensed};
		return

let do_release_license profile =
	if not (Hashtbl.mem licenses_checked_out profile)
	then false (* license not checked out for profile *)
	else begin
		let l = Hashtbl.find licenses_checked_out profile in
		incr l.quantity ;
		Hashtbl.remove licenses_checked_out profile ;
		true
	end

let release_license_c profile =
	match !state with
	| Some ({licensed = true; profile = profile} as s) ->
		info "Releasing mock license; profile: %s" profile;
		let result = do_release_license profile in
		state := Some {s with profile = ""; licensed = false};
		result
	| _ ->
		debug "No license to release";
		false

(* Is a license with (product, edition, dbv) present? Used for XD licenses. *)
let license_check_c address port product edition dbv =
	info "Checking for %s %s" product edition;
	(* let p = profile_of product edition in *)
	let p = prod_of_string product in
	let num_licenses = List.(!licenses
		|> filter (fun l -> l.prod = p && !(l.quantity) > 0)
		|> length) in
	let server_up = is_server_up () in
	debug "server up? %b maching licenses: %d" server_up num_licenses ;
	match (is_server_up ()), num_licenses with
	| false, _ -> debug "Server unreachable"; 0
	| true, n when n > 0 -> debug "License present"; 2
	| true, _ -> debug "License not present"; 1

(* This is currently unused in realv6. *)
let component_status_c s1 s2 = 0

(* XXX hours until grace license expires. Don't care about this right now. *)
let get_grace_info_c s = 30 * 24
