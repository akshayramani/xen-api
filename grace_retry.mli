(** Helper to keep trying to get a "real" license after a "grace" license was checked out. *)

(** Schedule a timer to call [Host.apply_edition] again after an hour. Call this
 *  after getting a "grace" license in order to check whether the license server
 *  happened to come back. If so, a "real" license will be checked out.
 *  Note: the LPE already does a similar thing, but does not notify the product (xapi)
 *  if it succeeds to check out a "real" license! *)
val start: string -> unit

(** Stop the retry thread, if it is running. *)
val cancel: unit -> unit

