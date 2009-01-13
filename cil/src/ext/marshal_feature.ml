let marshal (f : Cil.file) : unit =
  let cfn = f.Cil.fileName in
  let mfn = (String.sub cfn 0 (String.length cfn - 2)) ^ ".marshal" in
  let och = open_out_bin mfn in
  Marshal.to_channel och f [];
  flush och;
  close_out och

let feature : Cil.featureDescr = {
  Cil.fd_enabled = ref false;
  Cil.fd_name = "marshal";
  Cil.fd_description = "Serialize the CIL data structure to a .marshal file";
  Cil.fd_extraopt = [];
  Cil.fd_doit = marshal;
  Cil.fd_post_check = false;
}
