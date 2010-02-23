(** Bindings to the License Policy Engine (LPE) C-code *)

(** Citrix proprietary code *)
val _proprietary_code_marker : string

(** Represents the result of {!get_license}. *)
type checkout_result_t =
	| Granted_real		(** A real license was checked out *)
	| Granted_grace		(** A grace license was granted *)
	| Unreachable		(** No license was granted, because the license server was unreachable *)
	| Rejected			(** The license server rejected the checkout request *)

(** Indicated when a given license will expire. *)
type expiry_t =
	| Permanent		(** The license is permanent and will never expire *)
	| Days of int	(** The license will expire in the given number of days *)

(** Location of the cache directory of the LPE. *)
val v6_cache_dir : string

(** Allocate memory and set up thread for callbacks.
 *  This needs to be done before starting the LPE. *)
val init : unit -> unit

(** Initialise and start LPE. *)
val start: string -> int -> string -> string -> string -> bool

(** Check out license. *)
val get_license: unit -> checkout_result_t option * expiry_t option

(** Release license, if holding one. *)
val release_license: unit -> bool

(** Shut down and clean up LPE. *)
val stop: unit -> bool

