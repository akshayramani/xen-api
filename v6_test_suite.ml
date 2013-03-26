open OUnit

let (@@) f a = f a

(*
Things we want to test:

  - [x] the correct features appear in free vs paid
  - [x] either enterprise-xd or xendesktop string becomes XenDesktop edition
  - [ ] licensing N socket host acquires N licenses iff they are present
  - [ ] licensing N socket host acquires 0 licenses iff <N are present

*)

let test_enterprise_xd_works () =
	let open Edition in 
	"enterprise-xd should give XenDesktop edition" @?
		(of_string "enterprise-xd" = XenDesktop)

let v6_suite = "v6_suite" >::: [
	"test_all_free_features_in_free" >:: Edition.test_all_free_features_in_free ;
	"test_all_additional_free_features_in_free" >::
		Edition.test_all_additional_free_features_in_free;
	"test_all_additional_features_in_paid" >::
		Edition.test_all_additional_features_in_paid ;
	"test_enterprise_xd_works" >:: test_enterprise_xd_works ;
	] ;;

let _ = run_test_tt_main v6_suite ;;
