let _proprietary_code_marker = "Citrix proprietary code"

module D=Debug.Debugger(struct let name="reapply" end)
open D
open Threadext
open Client

let xapirpc xml =
	let open Xmlrpc_client in
	XMLRPC_protocol.rpc ~srcstr:"v6" ~dststr:"xapi" ~transport:(Unix "/var/xapi/xapi") ~http:(xmlrpc ~version:"1.0" "/") xml

let period =
	if Xapi_fist.reduce_grace_retry_period () then
		300.	(* 5min *)
	else
		3600.	(* 1h *)

(* This function sends a new host.apply_edition request to xapi *)
let reapply edition =
	let now = (Unix.gettimeofday ()) in
	let host_uuid = Xapi_inventory.lookup Xapi_inventory._installation_uuid in
	let session = Client.Session.login_with_password ~rpc:xapirpc ~uname:"" ~pwd:""
		~version:Xapi_globs.api_version_string in
	Pervasiveext.finally
		(fun () -> (* Retry checkout *)
			let host = Client.Host.get_by_uuid xapirpc session host_uuid in
			Client.Host.apply_edition xapirpc session host edition;
			(* Remove any newly generated grace alerts *)
			let alerts = Client.Message.get_since xapirpc session (Date.of_float now) in
			let check_and_maybe_remove (ref, msg) =
				if msg.API.message_name = "GRACE_LICENSE" then
					Client.Message.destroy xapirpc session ref
			in
			List.iter check_and_maybe_remove alerts
		)
		(fun () -> Client.Session.logout xapirpc session)

let running = ref false
let m = Mutex.create ()
let delay = Delay.make ()

(* Function that waits in a thread for a specified time, unless cancelled,
 * and then reapplies the license.
 * Note that if the re-apply may cause a new re-apply thread to be started,
 * which briefly overlaps with the original thread (which dies after the
 * [reapply] call). *)
let reapply_thread edition =
	Mutex.execute m (fun () -> running := true);
	debug "Will re-apply the license in %d seconds." (int_of_float period);
	if Delay.wait delay period then begin
		debug "Re-applying %s license..." edition;
		Mutex.execute m (fun () -> running := false);
		reapply edition
	end else begin
		debug "Stopping re-apply thread.";
		Mutex.execute m (fun () -> running := false)
	end

(* Start the retry thread. *)
let start edition =
	Mutex.execute m (fun () ->
		if !running = false then
			ignore (Thread.create reapply_thread edition)
	)

(* Stop the retry thread, if it is running. *)
let cancel () =
	Mutex.execute m (fun () ->
		if !running = true then
			Delay.signal delay
	)

