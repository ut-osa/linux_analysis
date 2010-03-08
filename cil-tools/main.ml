open Cil

let dir = Sys.argv.(1)
let ls dir = Array.to_list (Array.map (Filename.concat dir) (Sys.readdir dir))
let contents = ls dir
let rec expand_contents c =
   match c with
      | [] -> []
      | hdr :: cdr ->
         if Sys.is_directory hdr then
            List.concat [(expand_contents (ls hdr)); expand_contents cdr]
         else  hdr :: expand_contents cdr
let contents = expand_contents contents
let num_files = List.length contents
let fi = ref 0
let files = List.map (fun filename ->
      incr fi;
      print_string (filename ^ " "); 
      print_int !fi; print_char '/'; print_int num_files;
      print_newline ();
      let file = (Frontc.parse filename) () in
      let () = Rmtmps.removeUnusedTemps file in
      file) contents
let () = print_endline "MERGING"
let kernel = Mergecil.merge files "test"
let oc = open_out "blah.c";;
dumpFile !printerForMaincil oc "blah" kernel

class listFnVisitor = object(self)
   inherit nopCilVisitor
   method vfunc (f : fundec) =
      print_string f.svar.vname;
      print_newline ();
      DoChildren
end

let () = visitCilFileSameGlobals (new listFnVisitor) kernel
