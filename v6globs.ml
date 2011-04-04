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
