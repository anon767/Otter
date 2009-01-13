open Cil

let file = ref Cil.dummyFile
;;

let globals () = (!file).globals
;;
