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

let rec make_kptr t =
   match t with
   | TPtr (pt, a) -> let baset = unrollType pt in (
      match baset with
      | TFun _ -> t
      | _ -> 
         let pt = make_kptr pt in
         let KPtr (id, ot, kt) = kptr_typ pt in
         kt
   )
   | TArray (t, e, a) -> TArray (make_kptr t, e, a)
   | t -> t


let anon_field f =
   Str.string_match (Str.regexp_string "__annonCompField") f.fname 0

let anon_comp c =
   Str.string_match
      (Str.regexp "__anon\\(struct\\|union\\)____missing_field_name_")
      c.cname 0

let safe_cpp name =
   match name with
   | "private" -> "_C_private"
   | "class" -> "_C_class"
   | "this" -> "_C_this"
   | "true" -> "_C_true"
   | "false" -> "_C_false"
   | "not" -> "_C_not"
   | "typename" -> "_C_typename"
   | _ -> name

(* Cil printer class that expands anonymous unnamed inner struct/union *)
class anonPrinterClass = object(self)
   inherit defaultCilPrinterClass as super
   method pFieldDecl _ (f : fieldinfo) =
      match f.ftype with
      | TComp (comp, attrs) when anon_field f -> 
         let comp_new = copyCompInfo comp "" in
         super#pGlobal () (GCompTag (comp_new, f.floc))
      | _ -> super#pFieldDecl () f
end
let anonPrinter = new anonPrinterClass

let print_pre_def g =
   dumpGlobal !printerForMaincil !out_pre_defs g

let print_defn g =
   match g with
   | GCompTag (comp, loc) when anon_comp comp -> ()
   | GCompTag (comp, loc) -> 
      dumpGlobal anonPrinter !out_struct_list g
   | _ -> dumpGlobal !printerForMaincil !out_struct_list g

class printTypeDefs = object(self)
   inherit nopCilVisitor
   method vglob (g : global) =
      match g with
      | GType ({tname="bool"}, _) -> DoChildren
      | GType (_, _) -> print_pre_def g; DoChildren

      | GEnumTag _ -> print_pre_def g; DoChildren

      | GCompTag _
      | GCompTagDecl _
      | GEnumTagDecl _ -> print_defn g; DoChildren
      | g -> DoChildren
end

(* Change the name of fields to safe c++ identifiers.  Also unroll typedefs
 to avoid namespace issues *)
let safe_field fi = 
   {fcomp = fi.fcomp; fname = safe_cpp fi.fname;
    ftype = unrollTypeDeep fi.ftype;
    fbitfield = fi.fbitfield; fattr = fi.fattr; floc = fi.floc}

let rec safe_fields fields =
   match fields with
   | fi :: rest -> safe_field fi :: safe_fields rest
   | [] -> []

let rec safe_args args =
   match args with
   | (name, t, attr) :: rest -> (safe_cpp name, t, attr) :: safe_args rest
   | [] -> []

(* Change pointer fields to be kptr structs *)
let kptr_field fi =
   fi.ftype <- make_kptr fi.ftype; fi

let rec kptr_fields fields =
   match fields with
   | fi :: rest -> kptr_field fi :: kptr_fields rest
   | [] -> []

(* Go through all types, change structure names, field names, and
 replace with kptrs *)
class safeCppTypes = object(self)
   inherit nopCilVisitor
   method vtype (t : typ) = 
      match t with
      | TComp (comp, attr) ->
         comp.cname <- safe_cpp comp.cname;
         comp.cfields <- kptr_fields (safe_fields comp.cfields);
         ChangeTo (TComp (comp, attr))
      | TFun (ret, Some args, vararg, attr) ->
         ChangeTo (TFun (ret, Some (safe_args args), vararg, attr))
      | _ -> DoChildren

   method vglob (g : global) = 
      match g with
      | GCompTag (comp, loc) ->
         comp.cname <- safe_cpp comp.cname;
         comp.cfields <- kptr_fields (safe_fields comp.cfields);
         ChangeDoChildrenPost ([(GCompTag (comp, loc))], (fun x -> x))
      | GType ({tname=n; ttype=t; treferenced=r}, loc) ->
         ChangeTo ([GType ({tname=n; ttype=make_kptr t; treferenced=r}, loc)])
      | _ -> DoChildren
end

(* Print all kptr construction macros *)
let print_kptrs _ =
   Hashtbl.iter (fun k v -> 
      match v with 
      | KPtr (id, TVoid _, _) ->
         fprintf !out_kptrs "MAKE_VKPTR(%d, void);\n" id
      | KPtr (id, orig_type, _) -> 
         let td_g = GType ({tname="_kptr_" ^ (string_of_int id) ^ "_t";
                              ttype=orig_type; treferenced=false},
                           {line=(-1); file=""; byte=(-1)}) in
         let t_str =
            Pretty.sprint 80 (printGlobal !printerForMaincil () td_g) in
         fprintf !out_kptrs "MAKE_KPTR(%d, %s);\n" id t_str
      ) kptr_table

let print_typeapi cil_file = 
   out_struct_list := open_out "type_list.h";
   out_pre_defs := open_out "pre_defs.h";
   out_kptrs := open_out "kptr_list.h";
   visitCilFile (new safeCppTypes) cil_file;
   visitCilFileSameGlobals (new printTypeDefs) cil_file;
   print_kptrs ();
