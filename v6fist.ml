let _proprietary_code_marker = "Citrix proprietary code"

module D = Debug.Debugger(struct let name="v6fist" end)
open D

let fistpoint name = try Unix.access ("/tmp/fist_" ^ name) [ Unix.F_OK ]; true with _ -> false

let fistpoint_read name =
	try
		Some (Unixext.string_of_file ("/tmp/fist_" ^ name))
	with _ -> None

let delete name = Unixext.unlink_safe ("/tmp/fist_" ^ name)

(** Reduce the v6-licensing grace period from 30 days to 15 minutes *)
let reduce_grace_period () = fistpoint "reduce_grace_period"

(** Reduce the v6-licensing upgrade grace period from 4 days to 15 minutes *)
let reduce_upgrade_grace_period () = fistpoint "reduce_upgrade_grace_period"

(** Set the expiry date of a v6-license to the one in the file *)
let set_expiry_date () = fistpoint_read "set_expiry_date"

(** Reduce the retry period after obtaining a grace license from 1h to 5min *)
let reduce_grace_retry_period () = fistpoint "reduce_grace_retry_period"

(** Change the license re-apply period to a given number of seconds *)
let set_reapply_period () = fistpoint_read "set_reapply_period"

