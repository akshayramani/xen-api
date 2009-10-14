(** Implementation of the v6 licensing functionality

This module implements the XMLRPC function of the licensing daemon. It uses the
Licensing Policy Engine (LPE), a library provided by the Citrix licensing team,
to handle the communication with the license server. *)

val _proprietary_code_marker : string
(** Citrix proprietary code *)

val initialise : string -> int32 -> string -> string * int32
val shutdown : unit -> bool
val reopen_logs : unit -> bool

