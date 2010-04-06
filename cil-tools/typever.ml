open Cil
open Tools
open Printf

let n_types = ref 0
let type_table = Hashtbl.create 1024

let out_do_verify = ref stdout
let out_go_verify = ref stdout

let type_id t =
   let ts = typeSig t in
   try (Hashtbl.find type_table ts) with
   | Not_found ->
      let id = !n_types in
      Hashtbl.add type_table ts id;
      incr n_types;
      id

let typesig_cval ts =
   let tid = Hashtbl.find type_table ts in
   (string_of_int tid) ^ " /*" ^ (typesig_str ts) ^ "*/"

let type_id_cval t =
   (string_of_int (type_id t)) ^ " /*" ^ (type_str t) ^ "*/"

let print_comp_fn_begin comp =
   output_string !out_do_verify
      ("int do_verify_" ^ (type_id_cval comp) ^ " (void *v) {\n");
   output_string !out_do_verify "return 1\n"

let print_comp_fn_end comp = output_string !out_do_verify ";}\n\n"

let print_comp_field comp_t f =
   match f.fbitfield with
   | None ->
      let (bits, width) = bitsOffset comp_t (Field (f,NoOffset)) in
      let bytes = bits / 8 in
      let (ver_fn,ver_target) = (match f.ftype with
      | TPtr (t, _) -> ("ptr_verify", type_id_cval t)
      | _ -> ("go_verify", (type_id_cval f.ftype))) in
      ignore (fprintf !out_do_verify "&& %s((char*) v + %d, %s) /*%s*/\n"
         ver_fn bytes ver_target f.fname)
   | Some width -> 
      ignore (fprintf !out_do_verify "/*bitfield %s*/" f.fname)

class printTypeVer = object(self)
   inherit nopCilVisitor
   method vglob (g : global) =
      match g with
      | GCompTag (comp, loc) ->
         let comp_t = TComp (comp, []) in
         print_comp_fn_begin comp_t;
         List.iter (print_comp_field comp_t) comp.cfields;
         print_comp_fn_end comp_t;
         DoChildren
      | g -> DoChildren

end

let print_go_verify_type ts tid =
   match ts with
   | TSComp _ ->
      output_string !out_go_verify
         ("(type_id == " ^ (String.escaped (typesig_cval ts)) ^ " && ");
      output_string !out_go_verify
         ("do_verify_"^(string_of_int tid)^"(ptr)) ||\\\n");
   | ts -> ()

let print_go_verify () =
   output_string !out_go_verify
      "#define go_verify(ptr, type_id) (\\\n";
   Hashtbl.iter print_go_verify_type type_table;
   output_string !out_go_verify "0)\n"

let print_typever cil_file =
   out_do_verify := Tools.output_file "do_verify.c";
   out_go_verify := Tools.output_file "go_verify.h";
   output_string !out_do_verify "#include \"go_verify.h\"\n\n";
   visitCilFileSameGlobals (new printTypeVer) cil_file;

   print_go_verify ()
