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
(** Module that controls product edition and feature mappings.
 * @group Licensing
 *)

(** Available editions *)
type edition =
	| Free            (** XenServer Free Edition *)
	| Socket          (** XenServer Licensed Per-socket *)
	| XenDesktop      (** XenServer Enterprise Edition for XenDesktop *)

(** Raised by {!of_string} if the given string does not map to an edition. *)
exception Undefined_edition of string

(** Convert a string to an {!edition}. *)
val of_string : string -> edition

(** Convert an {!edition} to a string. *)
val to_string : edition -> string

(** Convert an {!edition} to an abbreviated string. *)
val to_short_string : edition -> string

(** Convert an {!edition} to its marketing name. *)
val to_marketing_name : edition -> string

(** Get the list of {!Feature.feature}s enabled for a given {!edition}. *)
val to_features : edition -> Features.feature list

(** Get the list of {!Additional_feature}s enabled for a given {!edition}. *)
val to_additional_features : edition -> Additional_features.feature list

(** Provides a total order. *)
val to_int : edition -> int

(** Compare two editions for equality (used before pool join). *)
val equal : edition -> edition -> bool

(** Return the "least capable" edition (used to determine the pool edition). *)
val min : edition list -> edition

(** Test that all free features are in the Free edition *)
val test_all_free_features_in_free : unit -> unit

(** Test that all additional free features are in the Free edition *)
val test_all_additional_free_features_in_free : unit -> unit

(** Test that all additional features are in the Socket edition *)
val test_all_additional_features_in_paid : unit -> unit
