(** Date-Based Version: burn-in date of current XenServer release (RTM date) *)
let dbv = "2010.0521"

(** Toggle between beta or GA licenses *)
let beta = true

let early_release =
	if beta then
		["earlyrelease", "true"]
	else
		[]

let v6product =
	if beta then
		"CXSTP"
	else
		"CXS"

(* Directory in which the LPE stores its cache *)
let v6_cache_dir = "/var/xapi/lpe-cache"

(* LPE config file and SA-date storage location *)
let lpe_ini = v6_cache_dir ^ "/LPE_LPE.ini"
let sa_date_filename = v6_cache_dir ^ "/sadate"
