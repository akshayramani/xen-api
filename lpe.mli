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

(** Convert a string to expiry_t *)
val expiry_t_of_string : string -> expiry_t

(** Convert an expiry_t to string *)
val string_of_expiry_t : expiry_t -> string

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

(** Check whether a {i component} is licensed. Each license has a list of components
 *  that it enables. This function checks for the presence of a license that has
 *  the given component, whether the license need not be checked out. The LPE does
 *  need to be {!start}ed with valid product and edition strings, before calling this
 *  function, and only license matching these will be checked. *)
val component_licensed: string -> checkout_result_t

(** Check for the presence of a license on the server. This function does not
 *  use the LPE, but the so-called helper APIs. *)
val license_check: string -> int -> string -> string -> string -> checkout_result_t

(** Returns the number of hours till the expiry of a grace license, if the LPE
 *  is running. This result is only useful when holding a grace license. *)
val get_grace_expiry: string -> int option

(** the SA date to a file *)
val write_sa_date: unit -> unit
