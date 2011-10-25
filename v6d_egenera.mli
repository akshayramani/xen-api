(** Main loop for the real v6 licensing daemon *)

(** Citrix proprietary code *)
val _proprietary_code_marker : string

(** Instantiate real v6 licensing daemon RPC handler *)
module P : sig 
	(** Process an RPC call *)
	val process : Rpc.call -> Rpc.response
end
