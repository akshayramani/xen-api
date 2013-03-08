(* We've functorised this module, so that we could mock out the LPE C
   stubs for the v6mockd. See lpe_functor.ml for all the LPE code. *)

include Lpe_functor.Make(Lpe_functor.Lpe_stubs)
