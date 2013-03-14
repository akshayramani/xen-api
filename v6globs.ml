(** Date-Based Version: burn-in date of current XenServer release (RTM date) *)
let dbv = "2010.0521"

let dbv =
	try
		let real_dbv_time = Lpe_helpers.timestamp_of_dbv_string dbv in
		let f = open_in "/tmp/dbv" in
		let fake_dbv = input_line f in
		let fake_dbv_time = Lpe_helpers.timestamp_of_dbv_string fake_dbv in
		if fake_dbv_time > real_dbv_time
		then fake_dbv
		else dbv
	with _ -> dbv

(** Toggle between beta or GA licenses *)
let beta = false

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
