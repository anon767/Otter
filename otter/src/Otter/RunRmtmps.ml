open Cil

let doit (file: Cil.file) =
    Rmtmps.removeUnusedTemps ~isRoot:(Rmtmps.isCompleteProgramRoot ~main_name:(!OtterCore.ProgramPoints.main_fname)) file

let feature : featureDescr = {
    fd_name = "RunRmtmps";
    fd_enabled = ref false;
    fd_description = "Run Cil's Rmtmps feature";
    fd_extraopt = [];
    fd_post_check = true;
    fd_doit = doit;
}
