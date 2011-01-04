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

open Edition
let supported_editions = [Free; Advanced; Enterprise; Enterprise_xd; Platinum]

let apply_edition edition additional_params =
	try
		let edition' = Edition.of_string edition in
		if List.mem edition' supported_editions then begin
			let name = Edition.to_marketing_name edition' in
			(* define "never" as 01-01-2030 *)
			let start_of_epoch = Unix.gmtime 0. in
			let never, _ = Unix.mktime {start_of_epoch with Unix.tm_year = 130} in
			let license =
				{
					(License.default ()) with
					License.sku = edition;
					License.sku_marketing_name = name;
					License.expiry = never;
				}
			in
			edition, Edition.to_features edition', License.to_assoc_list license
		end else
			raise (Edition.Undefined_edition edition)
	with Edition.Undefined_edition e ->
		raise (V6errors.Error (V6errors.invalid_edition, [edition]))

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

