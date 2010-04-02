open Cil
open Printf

let out_struct_list = ref stdout
let out_pre_defs = ref stdout
let out_kptrs = ref stdout

type kptr =
| KPtr of int * typ * typ

let n_kptrs = ref 0
let kptr_table = Hashtbl.create 1024

let kptr_fields t comp =
   [("ptr", voidType, None, [], {line=(-1); file=""; byte=(-1)})]

let kptr_typ t =
   let ts = typeSig t in
   try (Hashtbl.find kptr_table ts) with
   | Not_found ->
      let id = !n_kptrs in
      let kt = mkCompInfo true ("kptr_" ^ string_of_int id) (kptr_fields t) []
         in
      let k = KPtr (id, t, TComp (kt, [])) in
      Hashtbl.add kptr_table ts k;
      incr n_kptrs;
      k


let print_pre_def g =
   dumpGlobal !printerForMaincil !out_pre_defs g

let print_defn g =
   dumpGlobal !printerForMaincil !out_struct_list g

let rec make_kptr t =
   match t with
   | TPtr (t, a) -> (
      match t with
      | TFun _ -> t
      | _ -> 
         let t = make_kptr t in
         let KPtr (id, ot, kt) = kptr_typ t in
         kt
   )
   | TArray (t, e, a) -> TArray (make_kptr t, e, a)
   | t -> t

let safe_cpp name =
   match name with
   | "private" -> "_private"
   | _ -> name

let rec sub_kptrs fields comp =
   match fields with
   | fi :: rest ->
      (safe_cpp fi.fname, make_kptr fi.ftype, fi.fbitfield, fi.fattr, fi.floc) :: (sub_kptrs rest comp)
   | [] -> []

class makeKptrs = object(self)
   inherit nopCilVisitor
   method vglob (g : global) =
      match g with
      | GCompTag (comp, loc) ->
         ChangeTo [
            (GCompTag (mkCompInfo comp.cstruct comp.cname
                       (sub_kptrs comp.cfields) comp.cattr, loc))
         ]
      | g -> DoChildren
end

class printTypeDefs = object(self)
   inherit nopCilVisitor
   method vglob (g : global) =
      match g with
      | GType (t,loc) -> (match t.tname with
         | "bool" -> DoChildren
         | _ -> print_pre_def g; DoChildren)
      | GEnumTag _ -> print_pre_def g; DoChildren

      | GCompTag _
      | GCompTagDecl _
      | GEnumTagDecl _ -> print_defn g; DoChildren
      | g -> DoChildren
end


let print_kptrs _ =
   Hashtbl.iter (fun k v -> 
      match v with 
      | KPtr (id, TVoid _, _) ->
         fprintf !out_kptrs "MAKE_VKPTR(%d, void);\n" id
      | KPtr (id, orig_type, _) -> 
         let t_str =
            Pretty.sprint 80 (printType !printerForMaincil () orig_type) in
         fprintf !out_kptrs "MAKE_KPTR(%d, %s);\n" id t_str
      ) kptr_table

let print_typeapi cil_file = 
   out_struct_list := open_out "type_list.h";
   out_pre_defs := open_out "pre_defs.h";
   out_kptrs := open_out "kptr_list.h";
   visitCilFile (new makeKptrs) cil_file;
   visitCilFileSameGlobals (new printTypeDefs) cil_file;
   print_kptrs ();
