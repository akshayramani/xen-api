let _proprietary_code_marker = "Citrix proprietary code"

type checkout_result_t = Granted_real | Granted_grace | Unreachable | Rejected
type expiry_t = Permanent | Days of int

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
