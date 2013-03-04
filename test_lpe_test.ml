(* TODO Write unit tests for Lpe_test module *)

let test_read_mock_license () =
	(* write /tmp/mock.lic file *)
	(* call read_mock_license () *)
	(* test output *)
	(* delete /tmp/mock.lic file *)
	let lic = Lpe_test.read_mock_license ["tmp_mock.lic"] in
	ignore lic
