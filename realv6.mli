(** Implementation of the v6 licensing functionality

This module implements the XMLRPC function of the licensing daemon. It uses the
Licensing Policy Engine (LPE), a library provided by the Citrix licensing team,
to handle the communication with the license server. *)

module Make : functor (E : module type of Edition) ->
              functor (Lpe : module type of Lpe) -> V6rpc.V6api
