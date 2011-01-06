(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

module D=Debug.Debugger(struct let name="v6api" end)
open D

(* This module could do with some refactoring to reuse the almost identical code in realv6.ml! *)

open Edition
let supported_editions = [Free; Advanced; Enterprise; Enterprise_xd; Platinum]

let apply_edition edition additional =
	(* default is free edition with 30 day grace validity *)
	let default_license = License.default () in
	let current_edition = List.assoc "current_edition" additional in
	let startup = List.mem_assoc "startup" additional && List.assoc "startup" additional = "true" in
	let get_license edition current_license =
		try
			let edition' = Edition.of_string edition in
			if not (List.mem edition' supported_editions) then
				raise (Edition.Undefined_edition edition);
			let current_edition = if current_edition = "" then "free" else current_edition in
			match edition' with
			| Edition.Free ->
				if Edition.of_string current_edition = Edition.Free then begin
					let license_file =
						if List.mem_assoc "license_file" additional then
							List.assoc "license_file" additional
						else
							!License_file.filename
					in
					debug "License file: %s" !License_file.filename;
					begin try
						let new_license = License_file.do_parse_and_validate license_file in
						if license_file <> !License_file.filename then
							Unix.rename license_file !License_file.filename;
						info "Holding Free Edition license with expiry date %s."
							(Date.to_string (Date.of_float new_license.License.expiry));
						new_license
					with e ->
						if startup then match e with
							| License_file.License_expired l -> l (* keep expired license *)
							| _ ->
								(* activation file does not exist or is invalid *)
								if current_license.License.expiry < default_license.License.expiry then begin
									info "Existing free license with expiry date %s still in effect."
										(Date.to_string (Date.of_float current_license.License.expiry));
									{
										default_license with
										License.expiry = current_license.License.expiry
									}
								end else begin
									info "Generating Free Edition grace license, which needs to be activated in 30 days.";
									default_license
								end
						else match e with
							| License_file.License_expired l ->
								raise (V6errors.Error(V6errors.license_expired, []))
							| License_file.License_file_deprecated ->
								raise (V6errors.Error(V6errors.license_file_deprecated, []))
							| e ->
								begin
									debug "Exception processing license: %s" (Printexc.to_string e);
									raise (V6errors.Error(V6errors.license_processing_error, []))
								end
					end
				end else begin
					info "Downgrading from %s to free edition." current_edition;
					(* delete activation key, if it exists *)
					Unixext.unlink_safe !License_file.filename;
					default_license
				end
			| e ->
				if Edition.of_string current_edition = Edition.Free then
					info "Upgrading from free to %s edition..." edition
				else
					info "(Re)applying %s license..." edition;

				(* set expiry date (check fist point) *)
				let expires =
					(* CA-33155: FIST point may only set an expiry date earlier than the actual one *)
					begin match Xapi_fist.set_expiry_date () with
						| None ->
							(* define "never" as 01-01-2030 *)
							let start_of_epoch = Unix.gmtime 0. in
							let never, _ = Unix.mktime {start_of_epoch with Unix.tm_year = 130} in
							never
						| Some d ->
							Date.to_float (Date.of_string d)
					end
				in
				let name = Edition.to_marketing_name edition' in
				{
					default_license with
					License.sku = edition;
					License.sku_marketing_name = name;
					License.expiry = expires
				}
		with Edition.Undefined_edition e ->
			raise (V6errors.Error (V6errors.invalid_edition, [edition]))
	in
	let new_edition, new_license =
		(* If edition is blank, use Free as the default. This indicates startup after a fresh install,
		 * or the application of a new license file. *)
		let edition = if edition = "" then Edition.to_string Edition.Free else edition in
		try
			let current_license = License.of_assoc_list additional in
			if List.mem_assoc "license_file" additional && edition <> Edition.to_string Edition.Free then
				raise (V6errors.Error (V6errors.activation_while_not_free, []))
			else
				edition,
				get_license edition current_license
		with License.Missing_license_param _ ->
			(* No current license params: first boot -> give a default license.
			 * If an activation key exists, this will used. *)
			edition,
			get_license edition default_license
	in
	new_edition, Edition.to_features (Edition.of_string new_edition),
		(License.to_assoc_list new_license) @ V6globs.early_release @
		(Additional_features.to_assoc_list (Edition.to_additional_features (Edition.of_string new_edition)))

let get_editions () =
	List.map (fun e -> Edition.to_string e, Edition.to_marketing_name e,
		Edition.to_short_string e, Edition.to_int e) supported_editions

let get_version () =
	V6globs.dbv

let reopen_logs () =
	try
		debug "Reopening logfiles";
		Logs.reopen ();
		debug "Logfiles reopened";
		true
	with _ -> false

