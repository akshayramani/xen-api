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

let _proprietary_code_marker = "Citrix proprietary code"

module D=Debug.Debugger(struct let name="lpe" end)
open D

let (|>) a f = f a

type checkout_result_t = Granted_real | Granted_grace | Unreachable | Rejected
type expiry_t = Permanent | Days of int

let string_of_expiry_t = function
	| Permanent -> "Permanent"
	| Days d -> string_of_int d

let expiry_t_of_string = function
	| "Permanent" -> Permanent
	| d -> Days (int_of_string d)

(* The state of the LPE (copied from Lpe module) *)

type state_t =
	{ started: bool
	; address: string
	; port: int
	; product: string
	; edition: string
	; dbv: string
	; profile: string
	; licensed: bool }

type server_status_t = Unknown | Up | Down

let state = ref None (* None means "not initialised"; init needs to be called *)
let server_status = ref Unknown

let reset_state () =
	state := Some {started = false; address = ""; port = 0; product = "";
		edition = ""; dbv = ""; profile = ""; licensed = false};
	server_status := Unknown

(* License state, from init file *)

type license_t =
	(* { prod : [ `XD | `XS_SKT | `XS_ADV | `XS_ENT | `XS_PLT ] *)
	{ prod : [ `XD | `XS ]
	; ed : [ `SKT | `ADV | `ENT | `PLT ] option
	; quantity : int ref
	; expiry : float
	; sa_date : string }

let licenses = ref ([] : license_t list)
let licenses_checked_out = (Hashtbl.create 10 : (string, license_t) Hashtbl.t)

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

let init () =
	info "Initialising mock LPE" ;
	licenses := read_mock_license mock_license_files ;
	debug "Read %d mock licenses" (List.length !licenses)

let start address port product edition dbv =
	info "Starting mock LPE" ;
	init () ;
	(* if !state = None then init () ; *)
	state := Some {started = true; address = address; port = port; product = product;
	               edition = edition; dbv = dbv; profile = ""; licensed = false};
	true

let days_of_seconds s = s /. 86400. |> int_of_float

let days_until_expiry expiry =
	let seconds_until_expiry = expiry -. Unix.time () in
	Days (days_of_seconds seconds_until_expiry)

let is_server_up () =
	match read_state_file mock_state_files with
	| None -> true
	| Some (up, _, _) -> up

let string_of_prod = function
	| `XD -> "XD" | `XS -> "XS"

let checkout_license state profile =
	(* Get the license that matches product, that expires last, or None *)
	let product = prod_of_string state.product in
	let edition = edition_of_string state.edition in
	let license = List.(!licenses
		|> filter (fun l ->
			debug "XXX l.quantity=%d; l.prod=%s; product=%s; edition=%s"
				!(l.quantity) (string_of_prod l.prod) state.product state.edition ;
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

(* TODO this mimics current behaviour. Next we need to have this function
   check out multiple licenses, one for each socket *)
let get_license sockets =
	info "Attempting to get mock license" ;
	match !state with
	| Some {licensed = true} ->
		debug "Cannot get license: a license was already checked out";
		None, None
	| Some s when s.started ->
		let profile = s.product ^ "_" ^ s.edition ^ "_" ^ "_CCS#" ^ "test_profile_1" in
		debug "Getting mock license for profile %s" profile ;
		let result, expiry, licensed =
			match (checkout_license s profile) with
			| `rejected -> Rejected, None, false
			| `unreachable -> Unreachable, None, false
			| `granted_grace -> Granted_grace, None, true
			| `granted_real days -> Granted_real, Some days, true
		in
		state := Some {s with profile = profile; licensed = licensed};
		Some result, expiry
	| _ ->
		debug "Cannot get license: LPE is not running";
		None, None

let do_release_license profile =
	if not (Hashtbl.mem licenses_checked_out profile)
	then false (* license not checked out for profile *)
	else begin
		let l = Hashtbl.find licenses_checked_out profile in
		incr l.quantity ;
		Hashtbl.remove licenses_checked_out profile ;
		true
	end

let release_license () =
	match !state with
	| Some ({licensed = true} as s) ->
		info "Releasing mock license; profile: %s" s.profile;
		let result = do_release_license s.profile in
		state := Some {s with profile = ""; licensed = false};
		result
	| _ ->
		debug "No license to release";
		false

let stop () =
	match !state with
	| Some {started = true} ->
		info "Stopping mock LPE" ;
		ignore (release_license ()) ;
		reset_state () ;
		true
	| _ ->
		debug "Cannot stop mock LPE because it was never started";
		reset_state () ;
		false

(* XXX *)
let component_status product component = true

let component_licensed component =
	match !state with
	| Some ({started = true} as s) ->
		info "Checking component status; product %s, component: %s" s.product component;
		let licensed = component_status s.product component in
		(match !server_status, licensed with
		| Up, true ->
			info "Component is licensed";
			Granted_real
		| Up, false ->
			info "Component NOT licensed";
			Rejected
		| Down, true ->
			info "Component is GRACE licensed";
			Granted_grace
		| Down, false ->
			info "Communication problem";
			Unreachable
		| _ -> (* Pattern will never match, as server cannot be Unknown here *)
			Unreachable)
	| _ ->
		debug "Cannot check status: mock LPE is not running";
		Unreachable

(*let profile_of prod edition =
	match prod with
	| "XDS" | "XDT" | "CDIAB" -> `XD
	| "XS" -> (match edition with
		| "SKT" -> `XS_SKT
		| "ADV" -> `XS_ADV
		| "ENT" -> `XS_ENT
		| "PLT" -> `XS_PLT
		| e -> failwith ("unknown xenserver edition " ^ e))
	| p -> failwith ("unknown product " ^ p) *)

let license_check address port product edition dbv =
	info "Checking for %s %s" product edition;
	(* let p = profile_of product edition in *)
	let p = prod_of_string product in
	let num_licenses = List.(!licenses
		|> filter (fun l -> l.prod = p && !(l.quantity) > 0)
		|> length) in
	let server_up = is_server_up () in
	debug "server up? %b maching licenses: %d" server_up num_licenses ;
	match (is_server_up ()), num_licenses with
	| false, _ -> debug "Server unreachable"; Unreachable
	| true, n when n > 0 -> debug "License present"; Granted_real
	| true, _ -> debug "License not present"; Rejected

let get_grace_expiry product =
	match !state with
	| Some {started = true} ->
		debug "Checking grace expiry";
		let hours_left = 0 (* XXX get_grace_info_c product *) in
		debug "hours left: %d" hours_left;
		Some hours_left
	| _ ->
		debug "LPE is not running";
		None

let write_sa_date () = ()
