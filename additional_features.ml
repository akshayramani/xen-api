module D = Debug.Debugger(struct let name="license" end)
open D

(* Additional features and restrictions *)

type feature =
	| Vswitch_controller
	| Lab
	| Stage
	| StorageLink
	| StorageLink_site_recovery
	| Web_self_service
	| Web_self_service_manager
	| Hotfix_apply

(* The names of these flags _must_ start with "restrict_" *)
let keys_of_features =
	[
		Vswitch_controller, "restrict_vswitch_controller";
		Lab, "restrict_lab";
		Stage, "restrict_stage";
		StorageLink, "restrict_storagelink";
		StorageLink_site_recovery, "restrict_storagelink_site_recovery";
		Web_self_service, "restrict_web_selfservice";
		Web_self_service_manager, "restrict_web_selfservice_manager";
		Hotfix_apply, "restrict_hotfix_apply";
	]

let string_of_feature f =
	List.assoc f keys_of_features
	
let feature_of_string str =
	let f, _ = List.find (fun (_, k) -> str = k) keys_of_features in
	f

let all_features =
	List.map (fun (f, _) -> f) keys_of_features

let to_assoc_list (s: feature list) =
	let get_map f =
		let str = string_of_feature f in
		let switch = string_of_bool (not (List.mem f s)) in
		str, switch
	in
	List.map get_map all_features

let of_assoc_list l =
	let get_feature (k, v) =
		try
			let v = not (bool_of_string v) in
			let f = feature_of_string k in
			if v then Some f else None
		with _ ->
			None
	in
	let features = List.map get_feature l in
	List.fold_left (function ac -> function Some f -> f :: ac | None -> ac) [] features

