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

type edition = Free | Advanced | Enterprise | Enterprise_xd | Platinum
exception Undefined_edition of string

let of_string = function
	| "free" | "XE Express" -> Free
	| "advanced" -> Advanced
	| "enterprise" | "XE Enterprise" -> Enterprise
	| "enterprise-xd" -> Enterprise_xd
	| "platinum" -> Platinum
	| x -> raise (Undefined_edition x)

let to_string = function
	| Free -> "free"
	| Advanced -> "advanced"
	| Enterprise -> "enterprise"
	| Enterprise_xd -> "enterprise-xd"
	| Platinum -> "platinum"

let to_short_string = function
	| Free -> "FREE"
	| Advanced -> "ADV"
	| Enterprise -> "ENT"
	| Enterprise_xd -> "XD"
	| Platinum -> "PLT"
	
let to_marketing_name = function
	| Free -> "Citrix XenServer"
	| Advanced -> "Citrix XenServer Advanced Edition"
	| Enterprise -> "Citrix XenServer Enterprise Edition"
	| Enterprise_xd -> "Citrix XenServer for XenDesktop"
	| Platinum -> "Citrix XenServer Platinum Edition"

(* Editions to features *)

let free_features = [VLAN; QoS; Shared_storage; Pooling; Marathon; Connection; IntelliCache]
let advanced_features = HA :: Email :: Performance :: DMC :: CPU_masking :: No_nag_dialog :: No_platform_filter :: free_features
let enterprise_features = Netapp :: Equalogic :: WLB :: RBAC :: Checkpoint :: advanced_features
let platinum_features = DR :: VMPR :: enterprise_features

let additional_free_features = []
let additional_advanced_features = Vswitch_controller :: additional_free_features
let additional_enterprise_features = StorageLink :: Web_self_service :: additional_advanced_features
let additional_platinum_features = Lab :: Stage :: StorageLink_site_recovery :: Web_self_service_manager :: additional_enterprise_features

let to_features = function
	| Free -> free_features
	| Advanced -> advanced_features
	| Enterprise | Enterprise_xd -> enterprise_features
	| Platinum -> platinum_features

let to_additional_features = function
	| Free -> additional_free_features
	| Advanced -> additional_advanced_features
	| Enterprise | Enterprise_xd -> additional_enterprise_features
	| Platinum -> additional_platinum_features
	
let to_int = function
	| Platinum -> 30
	| Enterprise | Enterprise_xd -> 20
	| Advanced -> 10
	| Free | _ -> 0

let equal e0 e1 =
	to_int e0 = to_int e1

let min l =
	List.fold_left (fun m e -> if to_int e < to_int m then e else m) Platinum l

