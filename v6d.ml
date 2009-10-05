let _proprietary_code_marker = "Citrix proprietary code"

module P = V6xmlrpc.V6process(Realv6)

let _ =
	Logs.reset_all [ "file:/var/log/v6d.log" ];
	V6daemon.startup (fun () -> ignore(Lpe.init ())) P.process

