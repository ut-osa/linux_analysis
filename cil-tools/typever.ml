open Cil
open Tools
open Printf

let n_types = ref 0
let type_table = Hashtbl.create 1024

let out_do_verify = ref stdout
let out_go_verify = ref stdout
let out_type_offsets = ref stdout
let out_sym_types = ref stdout
let out_type_enum = ref stdout

let do_verify_set = Hashtbl.create 1024

let field_names = Hashtbl.create 1024
let n_field_names = ref 0

let noAttrTypeSig t = typeSigWithAttrs (fun x -> []) t

let find_type t =
   let ts = noAttrTypeSig t in Hashtbl.find type_table ts

let type_id t =
   let ts = noAttrTypeSig t in
   try Hashtbl.find type_table ts with
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

let field_name s =
   try Hashtbl.find field_names s with
   | Not_found ->
      let id = !n_field_names in
      Hashtbl.add field_names s id;
      incr n_field_names;
      id

let is_union comp_t = match comp_t with
   | TComp (comp, _) -> not comp.cstruct
   | _ -> false

let print_comp_fn_begin comp =
   let t_cval = type_id_cval comp in
   output_string !out_do_verify
      ("static int do_verify_" ^ t_cval ^ " (void *v) {\n");
   if is_union comp then
      output_string !out_do_verify "return 0\n"
   else
      output_string !out_do_verify "return 1\n";
   output_string !out_go_verify
      ("static int do_verify_" ^ t_cval ^ "(void*);\n");
   output_string !out_type_offsets
      ("typeoff_map_t typeoff_map_" ^ t_cval ^ "[] = { ")

let print_comp_fn_end comp =
   output_string !out_do_verify ";}\n\n";
   output_string !out_type_offsets "{-1, -1} };\n"

let print_comp_field comp_t f =
   match f.fbitfield with
   | None ->
      let (bits, width) = bitsOffset comp_t (Field (f,NoOffset)) in
      let bytes = bits / 8 in
      let bool_op = if is_union comp_t then "||" else "&&" in
      let ver_template = (match f.ftype with
      | TPtr (t, _) -> fprintf !out_do_verify
                        "%s ptr_verify(%s, %d, (char*)v + %d) /*%s*/\n"
                        bool_op (type_id_cval t) (field_name f.fname)
      | _ -> let t_cval = type_id_cval f.ftype in 
         fprintf !out_type_offsets "{%s, %d}, " t_cval bytes;
         fprintf !out_do_verify "%s go_verify_%s((char*)v + %d) /*%s*/\n"
               bool_op t_cval) in
      ver_template bytes f.fname
   | Some width -> 
      fprintf !out_do_verify "/*bitfield %s*/" f.fname

class printTypeVer = object(self)
   inherit nopCilVisitor
   method vglob (g : global) =
      match g with
      | GCompTag (comp, loc) ->
         let comp_t = TComp (comp, []) in
         Hashtbl.add do_verify_set (noAttrTypeSig comp_t) ();
         print_comp_fn_begin comp_t;
         List.iter (print_comp_field comp_t) comp.cfields;
         print_comp_fn_end comp_t;
         DoChildren
      | g -> DoChildren

end

let print_go_verify_type ts tid =
   let ts_cval = typesig_cval ts in
   fprintf !out_go_verify "static inline int go_verify_%s(char *ptr) {"
      ts_cval;
   match ts with
   | TSComp _ -> (
      try Hashtbl.find do_verify_set ts; 
          fprintf !out_go_verify " return do_verify_%s(ptr); }\n\n" ts_cval
      with Not_found -> output_string !out_go_verify " return 1; }\n\n"
   )
   | ts -> output_string !out_go_verify " return 1; }\n\n"

let print_go_verify () =
   Hashtbl.iter print_go_verify_type type_table

let print_verify_map () =
   output_string !out_do_verify "verify_fn_t verify_map[] = { ";
   output_string !out_type_offsets "\ntypeoff_map_t *all_typeoff_maps[] = {\n";
   let map_entry t _ = 
      let ts_cval = typesig_cval t in
      fprintf !out_do_verify "[%s] = do_verify_%s,\n" ts_cval ts_cval;
      fprintf !out_type_offsets "[%s] = typeoff_map_%s,\n" ts_cval ts_cval
      in
   Hashtbl.iter map_entry do_verify_set;
   output_string !out_do_verify "};\n";
   output_string !out_do_verify
      "int max_verify_fn = sizeof(verify_map)/sizeof(verify_map[0]);\n\n";
   output_string !out_type_offsets "};\n"

let print_field_names () =
   output_string !out_do_verify "const char *field_names[] = { ";
   let print_name f id =
      fprintf !out_do_verify "[%d] = \"%s\",\n" id f
      in
   Hashtbl.iter print_name field_names;
   output_string !out_do_verify "};\n\n"

class printSymTypes = object(self)
   inherit nopCilVisitor
   method vglob (g : global) =
      match g with
      | GVar (vi, _, _) -> 
         let ts = noAttrTypeSig vi.vtype in
         (try (
            Hashtbl.find do_verify_set ts;
            fprintf !out_sym_types "{\"%s\", %s},\n" vi.vname 
               (type_id_cval vi.vtype)
         ) with Not_found -> ());
         DoChildren
      | _ -> DoChildren
end

let print_sym_header _ =
   output_string !out_sym_types "#include \"typedefs.h\"\n\n";
   output_string !out_sym_types "struct symbol_type sym_types[] = {\n"

let print_sym_footer _ =
   output_string !out_sym_types "};\n\n";
   output_string !out_sym_types
      "int n_sym_types = sizeof(sym_types)/sizeof(sym_types[0]);\n"

let print_type_enum _ =
   output_string !out_type_enum "enum type_ids {\n";
   let print_type_id ts _ =
      let ts_cval = typesig_cval ts in
      let name = match ts with
         | TSComp (is_struct, x, _) ->
            (if is_struct then "STRUCT_" else "UNION_") ^ (String.uppercase x)
         | _ -> "BLAH"
         in
      fprintf !out_type_enum "%s = %s,\n" name ts_cval
      in
   Hashtbl.iter print_type_id do_verify_set;
   output_string !out_type_enum "};\n"

let print_typever cil_file =
   out_do_verify := Tools.output_file "do_verify.c";
   out_go_verify := Tools.output_file "go_verify.h";
   out_type_offsets := Tools.output_file "type_offsets.c";
   out_sym_types := Tools.output_file "symbol_types.c";
   out_type_enum := Tools.output_file "type_enum.h";
   output_string !out_do_verify
      "#include \"go_verify.h\"\n#include \"typedefs.h\"\n\n";
   output_string !out_type_offsets "typedef int typeoff_map_t[2];\n\n";
   visitCilFileSameGlobals (new printTypeVer) cil_file;

   print_go_verify ();
   print_verify_map ();
   print_field_names();

   print_sym_header ();
   visitCilFileSameGlobals (new printSymTypes) cil_file;
   print_sym_footer();

   print_type_enum ();

   ()
