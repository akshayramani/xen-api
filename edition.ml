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

type edition = Free | Socket | XenDesktop | Advanced | Enterprise | Platinum
exception Undefined_edition of string

let of_string = function
	| "free" | "XE Express" -> Free
	| "per-socket" -> Socket
	| "xendesktop" -> XenDesktop
	| "advanced" -> Advanced
	| "enterprise" | "XE Enterprise" -> Enterprise
	| "platinum" -> Platinum
	| x -> raise (Undefined_edition x)

let to_string = function
	| Free -> "free"
	| Socket -> "per-socket"
	| XenDesktop -> "xendesktop"
	| Advanced -> "advanced"
	| Enterprise -> "enterprise"
	| Platinum -> "platinum"

let to_short_string = function
	| Free -> "FREE"
	| Socket -> "STD"
	| XenDesktop -> "XD"
	| Advanced -> "ADV"
	| Enterprise -> "ENT"
	| Platinum -> "PLT"

let to_marketing_name = function
	| Free -> "Citrix XenServer"
	| Socket -> "Citrix XenServer Licensed"
	| XenDesktop -> "Citrix XenServer for XenDesktop"
	| Advanced -> "Citrix XenServer Advanced Edition"
	| Enterprise -> "Citrix XenServer Enterprise Edition"
	| Platinum -> "Citrix XenServer Platinum Edition"

(* Editions to features *)

let free_features =
	VLAN :: QoS :: Shared_storage :: Pooling :: Marathon :: Connection ::
	IntelliCache :: HA :: Email :: Performance :: DMC :: CPU_masking :: No_nag_dialog ::
	No_platform_filter :: VMPR :: VIF_locking :: Storage_motion ::
	Netapp :: Equalogic :: WLB :: RBAC :: Checkpoint :: GPU :: DR :: []

let additional_free_features = Lab :: Stage :: StorageLink_site_recovery :: StorageLink ::
	Web_self_service_manager :: Web_self_service :: Vswitch_controller :: []

let paid_features = [] (* will be [ Hotfix_apply ; RPU ] *)

let to_features = function
	| Free -> free_features
	| _ -> paid_features @ free_features

let to_additional_features _ = additional_free_features

let to_int = function
	| Platinum -> 30
	| Enterprise | XenDesktop -> 20
	| Advanced -> 10
	| Socket -> 5
	| Free -> 0

let equal e0 e1 =
	to_int e0 = to_int e1

let min l =
	List.fold_left (fun m e -> if to_int e < to_int m then e else m) Platinum l

(* Unit tests which test unexported values *)
let test_all_free_features_in_free () =
	OUnit.assert_bool "all free features in free" (free_features = (to_features Free))

let test_all_features_in_paid () =
	OUnit.assert_bool "all features in paid" (free_features @ paid_features = (to_features Socket))
