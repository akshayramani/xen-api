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
	| "advanced" -> Advanced
	| "enterprise" | "XE Enterprise" -> Enterprise
	| "xendesktop" -> XenDesktop
	| "platinum" -> Platinum
	| x -> raise (Undefined_edition x)

let to_string = function
	| Free -> "free"
	| Socket -> "per-socket"
	| Advanced -> "advanced"
	| Enterprise -> "enterprise"
	| XenDesktop -> "xendesktop"
	| Platinum -> "platinum"

let to_short_string = function
	| Free -> "FREE"
	| Socket -> "SKT"
	| Advanced -> "ADV"
	| Enterprise -> "ENT"
	| XenDesktop -> "XD"
	| Platinum -> "PLT"

let to_marketing_name = function
	| Free -> "Citrix XenServer"
	| Socket -> "Citrix XenServer Licensed"
	| Advanced -> "Citrix XenServer Advanced Edition"
	| Enterprise -> "Citrix XenServer Enterprise Edition"
	| XenDesktop -> "Citrix XenServer for XenDesktop"
	| Platinum -> "Citrix XenServer Platinum Edition"

(* Editions to features *)

let free_features = [VLAN; QoS; Shared_storage; Pooling; Marathon; Connection; IntelliCache; DMC]
let advanced_features = HA :: Email :: Performance :: CPU_masking :: No_nag_dialog :: No_platform_filter :: VMPR :: free_features
let enterprise_features = Netapp :: Equalogic :: WLB :: RBAC :: Checkpoint :: GPU :: advanced_features
let platinum_features = DR :: enterprise_features

let additional_free_features = []
let additional_advanced_features = Vswitch_controller :: additional_free_features
let additional_enterprise_features = StorageLink :: Web_self_service :: additional_advanced_features
let additional_platinum_features = Lab :: Stage :: StorageLink_site_recovery :: Web_self_service_manager :: additional_enterprise_features

let to_features = function
	| Free -> platinum_features
	| Advanced -> advanced_features
	| Enterprise | XenDesktop -> enterprise_features
	| Platinum | Socket -> platinum_features

let to_additional_features = function
	| Free -> additional_platinum_features
	| Advanced -> additional_advanced_features
	| Enterprise | XenDesktop -> additional_enterprise_features
	| Platinum | Socket -> additional_platinum_features
	
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

