open OcamlUtilities
open OtterCore

let main_loop get_job interceptor process_result job_queue =
	let rec main_loop job_queue completed =
		match get_job job_queue with
			| Some (job, job_queue) ->
				let result_opt =
					try
						let result, job_queue = interceptor job job_queue in
						let completed, job_queue = process_result result completed job_queue in
						Some (job_queue, completed)
					with Types.SignalException s ->
						(* if we got a signal, stop and return the completed results *)
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.printf "%s@\n" s;
						None
				in
				begin match result_opt with
					| Some (job_queue, completed) -> main_loop job_queue completed
					| None -> completed
				end
			| None ->
				completed
	in
	main_loop job_queue []
