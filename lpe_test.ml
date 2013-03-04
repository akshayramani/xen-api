(* Mock license file structure:
	<file> := <line>*
	<line> := <prod> <quantity> <expiration> <sa_date> <newline>
	        | <comment>
	<prod> := "XD" | "XS"
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
	{ prod : [ `XD | `XS ]
	; quantity : int ref
	; expiry : float
	; sa_date : string }

let licenses = ref ([] : license_t list)

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

let prod_of_string = function
	| "XD" -> `XD
	| "XS" -> `XS
	| _ -> failwith "Unknown product"

(* WARNING! Str not thread safe! Switch to re library ASAP! *)
let split = Str.(split (regexp "[ \t]+"))

let license_of_string s =
	let ss = split s in
	match ss with
	| comment :: _ when s.[0] = '#' -> None
	| [p; q; e; s] as line ->
		(try
			Some { prod = prod_of_string p
			     ; quantity = ref (int_of_string q)
			     ; expiry = float_of_string e
			     ; sa_date = s }
		with _ ->
			debug "Bad mock license line: %s" (String.concat " " line) ; None)
	| _ -> None

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
	debug "Initialising mock LPE" ;
	licenses := read_mock_license mock_license_files ;
	debug "Read %d mock licenses" (List.length !licenses)

let stop () =
	match !state with
	| Some {started = true} ->
		debug "Stopping mock LPE" ;
		reset_state () ;
		true
	| _ ->
		debug "Cannot stop mock LPE because it was never started";
		reset_state () ;
		false

let start address port product edition dbv =
	if !state = None then init () ;
	debug "Starting mock LPE" ;
	state := Some {started = true; address = address; port = port; product = product;
	               edition = edition; dbv = dbv; profile = ""; licensed = false};
	true

let days_of_seconds s = s /. 86400. |> int_of_float

let days_until_expiry expiry =
	let seconds_until_expiry = expiry -. Unix.time () in
	Days (days_of_seconds seconds_until_expiry)

(* TODO finish this. Possible states are:
	1) rejected license: rejected if not enough licenses of that type
	2) couldn't contact server: happens if mock state says license server is down
	3) license granted: right number of the right kind of licenses
	4) granted grace: grace license granted if license server is down, but we had
	   previously sucessfully checked out a license of the same type *)
let checkout_license state product =
	(* reread mock.state, in case we want to inject the license server going down *)
	let server_up, _, _ = match read_state_file mock_state_files with
		| None -> false, "", 0 | Some x -> x in
	(* Get the license that matches product, that expires last, or None *)
	let license = List.(!licenses
		|> filter (function | l when l.prod = product -> true | _ -> false)
		|> filter (function | l when !(l.quantity) = 0 -> true | _ -> false)
		|> sort (fun a b -> compare b.expiry a.expiry)
		|> function | hd :: _ -> Some hd | [] -> None)
	in
	match server_up, license with
	| false, _ -> `unreachable
	| true, None -> `rejected
	(* XXX when do we give grace licenses? *)
	| true, Some l -> begin
		decr l.quantity ;
		`granted_real (days_until_expiry l.expiry)
		end

(* TODO this mimics current behaviour. Next we need to have this function
   check out multiple licenses, one for each socket *)
let get_license () =
	match !state with
	| Some {licensed = true} ->
		debug "Cannot get license: a license was already checked out";
		None, None
	| Some s when s.started ->
		let product = prod_of_string s.product in
		let result, expiry, licensed =
			match (checkout_license !state product) with
			| `rejected -> Rejected, None, false
			| `unreachable -> Unreachable, None, false
			| `granted_grace -> Granted_grace, None, true
			| `granted_real days -> Granted_real, Some days, true
		in
		let profile = s.product ^ "_" ^ s.edition ^ "_" ^ "_CCS#" ^ "test_profile_1" in
		state := Some {s with profile = profile; licensed = licensed};
		Some result, expiry
	| _ ->
		debug "Cannot get license: LPE is not running";
		None, None

let (release_license: unit -> bool) = fun () -> true

let (component_licensed: string -> checkout_result_t) = fun _ -> Granted_real

let (license_check: string -> int -> string -> string -> string -> checkout_result_t) =
	fun address port product edition dbv -> Granted_real

let (get_grace_expiry: string -> int option) = fun _ -> None

let write_sa_date () = ()
