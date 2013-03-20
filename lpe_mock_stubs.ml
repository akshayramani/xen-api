let _proprietary_code_marker = "Citrix proprietary code"

(* Mock license file structure:
	<file> := <line>*
	<line> := <prod> <quantity> <expiration> <sa_date> <newline>
	        | <prod> <edition> <quantity> <expiration> <sa_date> <newline>
	        | <comment> <newline>
	<prod> := "XD" | "XS"
	<edition> := "STD" | "ADV" | "ENT" | "PLT"
	<quantity> := <positive integer>
	<expiration> := <float> (unix timestamp)
	<sa_date> := <float> (unix timestamp)
	<newline> := "\n"
	<comment> := <whitespace>* "#" <any string> *)

(* Look for mock license files in this order *)
let mock_license_files = [ "mock.lic"; "/tmp/mock.lic"]
let mock_state_files = [ "mock.state"; "/tmp/mock.state" ]

module D=Debug.Debugger(struct let name="lpe_mock_stubs" end)
open D

let (|>) a f = f a

let never = 1893456000. (* 1 Jan 2030 timestamp *)

type state_t =
	{ started: bool
	; address: string
	; port: int
	; dbv: float}

type license_t =
	{ prod : [ `XD | `XS ]
	; ed : [ `STD | `ADV | `ENT | `PLT ] option
	; quantity : int ref
	; expiry : float
	; sa_date : float }

let state = ref None
let licenses = ref ([] : license_t list)
let licenses_checked_out = (Hashtbl.create 10 : (string, license_t) Hashtbl.t)

let days_of_seconds s = s /. 86400. |> int_of_float (* seconds in day *)

let string_of_timestamp f =
    let open Unix in
    let d = gmtime f in
    Printf.sprintf "%4d.%02d%02d"
        (d.tm_year + 1900)
        (d.tm_mon + 1)
        d.tm_mday

let timestamp_of_dbv_string = Lpe_helpers.timestamp_of_dbv_string

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
	| "STD" -> `STD
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
			     ; expiry = timestamp_of_dbv_string x
			     ; sa_date = timestamp_of_dbv_string s }
		with _ ->
			debug "Bad mock license: %s" (String.concat " " line) ; None)
	| [p; e; q; x; s] as line ->
		(try
			Some { prod = prod_of_string p
			     ; ed = Some (edition_of_string e)
			     ; quantity = ref (int_of_string q)
			     ; expiry = timestamp_of_dbv_string x
			     ; sa_date = timestamp_of_dbv_string s }
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

let string_of_edition = function
	| Some `STD -> "STD" | Some `ADV -> "ADV"
	| Some `ENT -> "ENT" | Some `PLT -> "PLT"
	| None -> "None"

let print_license_state () =
	debug "Current licenses:" ;
	List.iter (fun l ->
		debug "     prod: %s; ed: %s; quant: %d; exp: %s; sa: %s"
			(string_of_prod l.prod)
			(string_of_edition l.ed)
			!(l.quantity)
			(string_of_timestamp l.expiry)
			(string_of_timestamp l.sa_date))
		!licenses ;
	debug "License profiles checked out:" ;
	Hashtbl.iter (fun p _ -> debug "     %s" p) licenses_checked_out

let split_profile profile =
	let i_prod = String.index profile '_' in
	let i_edit = String.index_from profile i_prod '_' in
	let product = String.sub profile 0 i_prod
	and edition = String.sub profile (i_prod+1) i_edit
	in
	debug "Splitting profile '%s': product: %s, edition: %s"
		profile product edition ;
	(prod_of_string product, edition_of_string edition)


(* callback_fd set by callback monitor thread in Lpe_functor, so protect
   it with a mutext and condition *)
let callback_m = Mutex.create ()
let callback_c = Condition.create ()
let callback_fd = ref None

let set_pipe i =
	debug "Setting callback pipe to %d" i ;
	Mutex.lock callback_m ;
	callback_fd := Some (Unixext.file_descr_of_int i) ;
	Condition.signal callback_c ;
	Mutex.unlock callback_m

let alloc_and_set_cache_dir d =
	debug "Prentending to set cache dir to %s" d

let write_to_callback_pipe msg =
	debug "Before lock callback_m" ;
	Mutex.lock callback_m ;
	if msg <> "" then
	while !callback_fd = None do
		debug "Waiting for callback_c condition" ;
		Condition.wait callback_c callback_m ;
	done ;
	(match !callback_fd with
	| None -> failwith "Callback thread not set"
	| Some fd -> Unixext.really_write fd msg 0 1) ;
	Mutex.unlock callback_m
	; debug "Unlocked callback_m mutex"

(* XXX deadlock here on first license apply after free! *)
let start_c address port product edition dbv =
	debug "Starting mock LPE engine" ;
	licenses := read_mock_license mock_license_files ;
	state := Some {started = true; address = address; port = port;
		dbv = timestamp_of_dbv_string dbv;};
	(* TODO if address,port are different than mock.state, we should return "d". *)
	write_to_callback_pipe "u" ;
	print_license_state () ;
	true

let checkout_license state profile =
	(* Get the license that matches product, that expires last, or None *)
	let product, edition = split_profile profile in
	let license = List.(!licenses
		|> filter (fun l ->
			   !(l.quantity) > 0
			&& l.prod = product
			&& l.ed = Some edition
			&& l.sa_date >= state.dbv)
		|> sort (fun a b -> compare b.expiry a.expiry)
		|> function | hd :: _ -> Some hd | [] -> None)
	in
	let server_up = is_server_up () in
	match server_up, license with
	| false, _ ->
		(* TODO Not sure how to simulate grace licenses in mock LPE *)
		(* if state.licensed
		then `granted_grace
		else *) `unreachable
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
		print_license_state () ;
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
	debug "Releasing license %s" profile ;
	match !state with
	| Some _ ->
		info "Releasing mock license; profile: %s" profile;
		let result = do_release_license profile in
		print_license_state () ;
		result
	| _ ->
		debug "No license to release";
		print_license_state () ;
		false

(* Is a license with (product, edition, dbv) present? Used for XD licenses. *)
let license_check_c address port product edition dbv =
	info "Checking for %s %s" product edition;
	print_license_state () ;
	let p = prod_of_string product in
	let dbv = timestamp_of_dbv_string dbv in
	let num_licenses = List.(!licenses
		|> filter (fun l -> l.prod = p && !(l.quantity) > 0 && l.sa_date >= dbv)
		|> length) in
	let server_up = is_server_up () in
	debug "server up? %b maching licenses: %d" server_up num_licenses ;
	match (is_server_up ()), num_licenses with
	| false, _ -> debug "Server unreachable"; 0
	| true, n when n > 0 -> debug "License present"; 2
	| true, _ -> debug "License not present"; 1

let stop_c () =
	debug "Stopping mock lpe engine" ;
	state := None ;
	Hashtbl.iter
		(fun a _ -> ignore (do_release_license a))
		licenses_checked_out ;
	print_license_state () ;
	true

(* This is currently unused in realv6. *)
let component_status_c s1 s2 = 0

(* Hours until grace license expires. Don't care about this right now. *)
let get_grace_info_c s =
	let r = 30 * 24 in
	debug "get_grace_info_c %s returns %d" s r ;
	r
