(* Factor out some common dependencies *)

let timestamp_of_dbv_string dbv =
	(* DBV string in form YYYY.MMDD *)
	let split = String.index dbv '.' in
	let y = String.sub dbv 0 split in
	let m = String.sub dbv (split+1) 2 in
	let d = String.sub dbv (split+3) 2 in
	let tm = Unix.gmtime 0. in
	let tm = try Unix.({tm with
			tm_year = (int_of_string y) - 1900;
			tm_mon = int_of_string m;
			tm_mday = int_of_string d; })
		with _ -> tm in
	fst (Unix.mktime tm)
