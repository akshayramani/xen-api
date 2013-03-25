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
let supported_editions = [Free; Socket; XenDesktop]

module L  = License.Make(Edition)
module LF = License_file.Make(Edition)

let apply_edition dbg edition additional =
	(* default is free edition with 30 day grace validity *)
	let default_license = L.default () in
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
				if startup || List.mem_assoc "license_file" additional then begin
					info "Attempting to apply 'free' edition activation key.";
					let license_file =
						if List.mem_assoc "license_file" additional then
							List.assoc "license_file" additional
						else
							!LF.filename
					in
					debug "License file: %s" !LF.filename;
					begin try
						let new_license = LF.do_parse_and_validate license_file in
						if license_file <> !LF.filename then
							Unix.rename license_file !LF.filename;
						info "Holding 'free' edition license with expiry date %s."
							(Date.to_string (Date.of_float new_license.L.expiry));
						new_license
					with 
					| LF.License_expired l when startup -> l (* keep expired license *)
					| _ when startup ->
						(* activation file does not exist or is invalid *)
						if current_license.L.expiry < default_license.L.expiry then begin
							info "Existing 'free' license with expiry date %s still in effect."
								(Date.to_string (Date.of_float current_license.L.expiry));
							{
								default_license with
								L.expiry = current_license.L.expiry
							}
						end else begin
							info "Generating 'free' edition grace license, which needs to be activated in 30 days.";
							default_license
						end
					| LF.License_expired l ->
						raise (V6errors.Error(V6errors.license_expired, []))
					| LF.License_file_deprecated ->
						raise (V6errors.Error(V6errors.license_file_deprecated, []))
					| e ->
						begin
							debug "Exception processing license: %s" (Printexc.to_string e);
							raise (V6errors.Error(V6errors.license_processing_error, []))
						end
					end
				end else if Edition.of_string current_edition = Edition.Free then begin
					info "The host's edition is already 'free', and not applying a new activation key or starting xapi. No change.";
					current_license
				end else begin
					info "Downgrading from '%s' to 'free' edition." current_edition;
					(* delete activation key, if it exists *)
					Unixext.unlink_safe !LF.filename;
					default_license
				end
			| e ->
				(* Ensure we are not trying to apply an old-style license file here. *)
				if List.mem_assoc "license_file" additional then
					raise (V6errors.Error (V6errors.activation_while_not_free, []));

				if Edition.of_string current_edition = Edition.Free then
					info "Upgrading from 'free' to '%s' edition..." edition
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
					L.sku = edition;
					L.sku_marketing_name = name;
					L.expiry = expires
				}
		with Edition.Undefined_edition e ->
			raise (V6errors.Error (V6errors.invalid_edition, [edition]))
	in
	let new_edition, new_license =
		(* If edition is blank, use Free as the default. This indicates startup after a fresh install,
		 * or the application of a new license file. *)
		let edition = if edition = "" then Edition.to_string Edition.Free else edition in
		try
			let current_license = L.of_assoc_list additional in
			edition,
			get_license edition current_license
		with L.Missing_license_param _ ->
			(* No current license params: first boot -> give a default license.
			 * If an activation key exists, this will used. *)
			edition,
			get_license edition default_license
	in
	new_edition, Edition.to_features (Edition.of_string new_edition),
		(L.to_assoc_list new_license) @ V6globs.early_release @
		(Additional_features.to_assoc_list (Edition.to_additional_features (Edition.of_string new_edition)))

let get_editions dbg =
	List.map (fun e -> Edition.to_string e, Edition.to_marketing_name e,
		Edition.to_short_string e, Edition.to_int e) supported_editions

let get_version dbg =
	V6globs.dbv

let reopen_logs () = true
