open OcamlUtilities
open OtterCore

let main_loop get_job interceptor process_result reporter job_queue =
	let rec main_loop reporter job_queue =
		match get_job job_queue with
			| Some (job, job_queue) ->
				let result_opt =
					try
						let job = { job with Job.steps = job.Job.steps + 1 } in
						let result, job_queue = interceptor job job_queue in
						let reporter, job_queue = process_result result reporter job_queue in
						Some (job_queue, reporter)
					with Types.SignalException s ->
						(* if we got a signal, stop and return the completed results *)
						Output.set_mode Output.MSG_MUSTPRINT;
						Output.printf "%s@\n" s;
						None
				in
				begin match result_opt with
					| Some (job_queue, reporter) -> main_loop reporter job_queue
					| None -> reporter
				end
			| None ->
				reporter
	in
	main_loop reporter job_queue
