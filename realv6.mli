(** Implementation of the v6 licensing functionality

This module implements the XMLRPC function of the licensing daemon. It uses the
Licensing Policy Engine (LPE), a library provided by the Citrix licensing team,
to handle the communication with the license server. *)

val _proprietary_code_marker : string
(** Citrix proprietary code *)

(** Obtain a license *)
val apply_edition : string -> (string * string) list ->
	string * Features.feature list * (string * string) list

(** Release the license *)
val get_editions : unit -> (string * string * string * int) list

val get_version : unit -> string

(** Close and re-open the log file *)
val reopen_logs : unit -> bool

