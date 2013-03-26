(** Module that controls feature restriction.
 * @group Licensing
 *)

(** Features than can be enabled and disabled. *)
type feature =
	| Vswitch_controller           (** Enable use of a Distributed VSwitch (DVS) Controller *)
	| Lab                          (** Enable use of Lab Manager *)
	| Stage                        (** Enable use of Stage Manager *)
	| StorageLink                  (** Enable use of StorageLink *)
	| StorageLink_site_recovery    (** Enable use of StorageLink Site Recovery *)
	| Web_self_service             (** Enable use of Web Self-Service *)
	| Web_self_service_manager     (** Enable use of Web Self-Service Manager *)
	| Hotfix_apply                 (** Enable GUI hotfix application *)

(** The list of all known features. *)
val all_features : feature list

(** Convert a {!feature} list into an association list. *)
val to_assoc_list : feature list -> (string * string) list

(** Convert an association list of features into a {!feature} list. *)
val of_assoc_list : (string * string) list -> feature list

