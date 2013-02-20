let _proprietary_code_marker = "Citrix proprietary code"

let (|>) a f = f a

type checkout_result_t = Granted_real | Granted_grace | Unreachable | Rejected
type expiry_t = Permanent | Days of int

(* Mock license file structure:
	<file> := <line>*
	<line> := <prod> <number> <expiration> <sa_date> <newline>
	<prod> := "XD" | "XS"
	<number> := <positive integer>
	<expiration> := "YYYYMMDD"
	<sa_date> := "YYYYMMDD"
	<newline> := "\n"
*)

type license = { product : [ `XD | `XS ]
               ; quantity : int
               ; expiry : string
               ; sa_date : string }

let ls_file_names = [ "mock.lic"; "/tmp/mock.lic"]

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

let read_mock_file () =
	let fi = open_file ls_file_names in
	match fi with
	| None -> []
	| Some fi ->
		let lines = read_lines fi in
		close_in fi ;
		lines

let product_of_string = function
	| "XD" -> `XD
	| "XS" -> `XS
	| _ -> failwith "Unknown product"

let license_of_string s =
	(* WARNING! Str not thread safe! Switch to re library ASAP! *)
	let ss = Str.(split (regexp "[ \t]+") s) in
	match ss with
	| [p; q; e; s] ->
		Some { product = product_of_string p
		     ; quantity = int_of_string q
		     ; expiry = e
		     ; sa_date = s }
	| _ -> None

let read_mock_license () =
	read_mock_file ()
		|> List.map license_of_string
		|> List.filter ((<>) None)
		|> List.map (function | Some x -> x | _ -> failwith "Unpossible!")

(* init will parse the fake license file *)
let init () =
	()

let stop () = true

let (start: string -> int -> string -> string -> string -> bool) =
	fun address port product edition dbv -> true

let string_of_expiry_t = function
	| Permanent -> "Permanent"
	| Days d -> string_of_int d

let expiry_t_of_string = function
	| "Permanent" -> Permanent
	| d -> Days (int_of_string d)

let (get_license: unit -> checkout_result_t option * expiry_t option) =
	fun () -> None, None

let (release_license: unit -> bool) = fun () -> true

let (component_licensed: string -> checkout_result_t) = fun _ -> Granted_real

let (license_check: string -> int -> string -> string -> string -> checkout_result_t) =
	fun address port product edition dbv -> Granted_real

let (get_grace_expiry: string -> int option) = fun _ -> None

let (write_sa_date: unit -> unit) = fun () -> ()
