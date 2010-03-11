open Cil
open Allocs
open Typever
open Machine32


(* Put the contents of dir in a list *)
let ls dir = Array.to_list (Array.map (Filename.concat dir) (Sys.readdir dir))

(* Expand the directory tree by listing subdirectories *)
let rec expand_contents c =
   match c with
      | [] -> []
      | hdr :: cdr ->
         if Sys.is_directory hdr then
            List.concat [(expand_contents (ls hdr)); expand_contents cdr]
         else  hdr :: expand_contents cdr

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
   let num_files = List.length contents in
   let fi = ref 0 in
   let parsed_files = List.map (fun filename ->
         incr fi;
         prerr_string (filename ^ " "); 
         prerr_int !fi; prerr_char '/'; prerr_int num_files;
         prerr_newline ();
         let file = (Frontc.parse filename) () in
         let () = Rmtmps.removeUnusedTemps file in
         file) contents
   in

   (* Merge all files into one *)
   let kernel = prerr_endline "MERGING"; Mergecil.merge parsed_files "test" in

   (* Dump the processed C *)
   let oc = open_out "blah.c" in
   dumpFile !printerForMaincil oc "blah" kernel;

   (* Run the analysis *)
   flush stderr;
   prerr_endline "ANALYZING";
   print_typever kernel
