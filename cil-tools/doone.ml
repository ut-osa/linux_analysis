(*
 * linux_analysis project
 * File name: doone.ml
 * 
 * Description: 
 * 
 * Operating Systems & Architecture Group
 * University of Texas at Austin - Department of Computer Sciences
 * Copyright 2010, 2011. All Rights Reserved.
 * See LICENSE file for license terms.
 *)

open Cil
open Machine

let () = 
   (* Change machine model *)
   let machineModel =
      try (Some (Machdepenv.modelParse (machine_str)))
      with Not_found -> None in
   Cil.envMachine := machineModel;

   let filename = Sys.argv.(1) in
   let parsed = (Frontc.parse filename) () in
   let () = Rmtmps.removeUnusedTemps parsed in
   let c_out = open_out_bin (filename ^ ".cil") in
   Marshal.to_channel c_out parsed []
