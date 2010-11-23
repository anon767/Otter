let log_buffer = Buffer.create 16

let formatter = ref (Format.formatter_of_buffer log_buffer)

let log_file = ref ""

let log_to_file filename =
    log_file := filename;
    formatter := Format.formatter_of_out_channel (open_out filename)

let log message = Format.fprintf !formatter "%s\n" message

let get_log () = if !log_file = "" then Buffer.contents log_buffer else "See the error log file: " ^ !log_file

let options = [
    "--error-log",
        Arg.String log_to_file,
        "<filename> Write messages about assertion failures to this file (rather than to stdout)";
]
