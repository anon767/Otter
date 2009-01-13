open Cil

let enable_log = ref false;;
let log_internal_ = ref "";;

exception Error of string;;

let log message =
	log_internal_ := ((!log_internal_)^message^"\n");
	()
	;;

let get_log () = (!log_internal_);;

let warn message something =
	Output.print_endline message;
	something
	;;