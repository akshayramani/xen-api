let _proprietary_code_marker = "Citrix proprietary code"

module D=Debug.Debugger(struct let name="v6mockd" end)

module Lpe_mock = Lpe_functor.Make(Lpe_mock_stubs)

module P = V6rpc.V6process(Realv6.Make(Edition)(Lpe_mock))

let handle_shutdown () =
	Sys.set_signal Sys.sigterm
		(Sys.Signal_handle
			 (fun _ ->
				 D.debug "v6mockd caught SIGTERM; performing cleanup actions." ;
				 D.debug "Writing SA date to file." ;
				 Lpe_test.write_sa_date () ;
				 exit 0 ))

let _ =
	Debug.set_facility Syslog.Local5;
	handle_shutdown ();
	V6daemon.startup Lpe_test.init P.process