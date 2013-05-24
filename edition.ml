(* (C) 2006-2010 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Features
open Additional_features

(* Editions definitions *)

type edition = Free | Socket | XenDesktop
exception Undefined_edition of string

let of_string = function
	| "free" | "XE Express" -> Free
	| "per-socket" -> Socket
	| "enterprise-xd"
	| "xendesktop" -> XenDesktop
	| x -> raise (Undefined_edition x)

let to_string = function
	| Free -> "free"
	| Socket -> "per-socket"
	| XenDesktop -> "xendesktop"

let to_short_string = function
	| Free -> "FREE"
	| Socket -> "STD"
	| XenDesktop -> "XD"

let to_marketing_name = function
	| Free -> "Citrix XenServer"
	| Socket -> "Citrix XenServer Per-Socket"
	| XenDesktop -> "Citrix XenServer for XenDesktop"

(* Editions to features *)

let free_features =
	VLAN :: QoS :: Shared_storage :: Pooling :: Marathon :: Connection ::
	IntelliCache :: HA :: Email :: Performance :: DMC :: CPU_masking :: No_nag_dialog ::
	No_platform_filter :: VMPR :: VIF_locking :: Storage_motion ::
	Netapp :: Equalogic :: RBAC :: Checkpoint :: GPU :: DR :: []

let additional_free_features = [Lab; Stage; StorageLink_site_recovery;
	StorageLink; Vswitch_controller]

let additional_paid_features = [Hotfix_apply]

let to_features _ = free_features

let to_additional_features = function
	| Free -> additional_free_features
	| _ -> additional_free_features @ additional_paid_features

let to_int = function
	| XenDesktop -> 10
	| Socket -> 5
	| Free -> 0

let equal e0 e1 =
	to_int e0 = to_int e1

let min l =
	List.fold_left (fun m e -> if to_int e < to_int m then e else m) XenDesktop l

(* Unit tests which test unexported values *)

let test_all_free_features_in_free () =
	let open OUnit in
	List.(iter (fun f ->
		"all free features in free" @? (mem f (to_features Free)))
		free_features)

let test_all_additional_free_features_in_free () =
	let open OUnit in
	List.(iter (fun f ->
		"all additional free features in free" @? (mem f (to_additional_features Free)))
		additional_free_features) ;
	"Hotfix_apply not in free" @? List.(not (mem Hotfix_apply (to_additional_features Free)))

let test_all_additional_features_in_paid () =
	let open OUnit in
	List.(iter (fun f ->
		"all features in paid" @? (mem f (to_additional_features Socket)))
		(additional_free_features @ additional_paid_features)) ;
	"Hotfix_apply in paid" @? List.(mem Hotfix_apply (to_additional_features Socket))
