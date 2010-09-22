open Cil
open Allocs
open Typever
open Typeapi
open Machine
open Tools


(* Put the contents of dir in a list *)
let ls dir = Array.to_list (Array.map (Filename.concat dir) (Sys.readdir dir))

(* Expand the directory tree by listing subdirectories *)
let rec expand_contents c =
   match c with
      | [] -> []
      | hdr :: cdr ->
         if Tools.is_directory hdr then
            List.concat [(expand_contents (ls hdr)); expand_contents cdr]
         else  hdr :: expand_contents cdr

let rec prune_files file_list =
   match file_list with
   | [] -> []
   | None :: rest -> prune_files rest
   | Some f :: rest -> f :: prune_files rest

let () = 
   (* Change machine model *)
   let machineModel =
      try (Some (Machdepenv.modelParse (machine_str)))
      with Not_found -> None in
   Cil.envMachine := machineModel;

   (* Get all files *)
   let dir = Sys.argv.(1) in
   let contents = ls dir in
   let contents = expand_contents contents in

   (* Parse each file *)
   let parsed_files = prune_files (progress_map "load" (fun filename ->
         prerr_string (Filename.basename filename); 
         match Filename.basename filename with
         | "i.tmp_vdso32-setup.o.i" -> 
            prerr_string " SKIPPING"; None
         | _ -> 
            (* let file = (Frontc.parse filename) () in *)
            let in_c = open_in_bin filename in
            let file = (Marshal.from_channel in_c : Cil.file) in
            let () = close_in in_c in
            (* let () = Rmtmps.removeUnusedTemps file in *)
            Some file
         ) contents)
   (* let parsed_files = prune_files (List.map (fun filename ->
         incr fi;
         prerr_int !fi; prerr_char '/'; prerr_int num_files;
         prerr_string (" " ^ Filename.basename filename); 
         match Filename.basename filename with
         | "i.tmp_vdso32-setup.o.i" -> 
            prerr_endline " SKIPPING"; None
         | _ -> 
            prerr_newline ();
            (* let file = (Frontc.parse filename) () in *)
            let in_c = open_in_bin filename in
            let file = (Marshal.from_channel in_c : Cil.file) in
            let () = close_in in_c in
            (* let () = Rmtmps.removeUnusedTemps file in *)
            Some file
         ) contents) *)
   in

   let () = Cil.initCIL () in


   (* Merge all files into one *)
   (* let kernel = prerr_endline "MERGING"; Mergecil.merge parsed_files "test" in *)

   (* Dump the processed C *)
   (* let oc = open_out "blah.c" in
   dumpFile !printerForMaincil oc "blah" kernel; *)

   (* Run the analysis *)
   flush stderr;
   prerr_endline "ANALYZING";
   print_typever parsed_files;
   print_all_allocs parsed_files;
   print_typeapi parsed_files
