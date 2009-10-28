(** Main loop for the real v6 licensing daemon *)

(** Citrix proprietary code *)
val _proprietary_code_marker : string

(** Instatiate real v6 licensing daemon XML/RPC handler *)
module P : sig 
	(** Process an XML/RPC call *)
	val process : XMLRPC.xmlrpc -> XMLRPC.xmlrpc
end
