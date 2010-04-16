open Cil
open Printf

let out_type_list = ref stdout

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
   | "private"
   | "class"
   | "this"
   | "true"
   | "false"
   | "not"
   | "typename"
   | "new"
   | "delete"
   | "bool"
   | "export"
   | "template"
   | "virtual" -> ("_C_" ^ name)
   | _ -> name

let cpp_builtin name =
   match name with
   | "bool"
   | "wchar_t" -> true
   | _ -> false

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

let print_defn g =
   match g with
   | GCompTag (comp, loc) when anon_comp comp -> ()
   | GCompTag (comp, loc) -> 
      dumpGlobal anonPrinter !out_type_list g
   | _ -> dumpGlobal !printerForMaincil !out_type_list g

class printEnums = object(self)
   inherit nopCilVisitor
   method vglob (g : global) =
      match g with
      | GEnumTag _ -> print_defn g; DoChildren
      | _ -> DoChildren
end

class printTypeDefs = object(self)
   inherit nopCilVisitor
   method vglob (g : global) =
      match g with
      (*
      | GType ({tname=n}, _) when (cpp_builtin n) -> DoChildren
      | GType _ -> print_defn g; DoChildren
      *)

      (* | GEnumTag _ -> print_pre_def g; DoChildren *)

      | GCompTag _
      | GCompTagDecl _ -> print_defn g; DoChildren
      (* | GEnumTagDecl _ -> print_defn g; DoChildren *)
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


(* Change pointer fields to be kptr structs *)
let kptr_field fi =
   fi.ftype <- make_kptr fi.ftype; fi

let rec kptr_fields fields =
   match fields with
   | fi :: rest -> kptr_field fi :: kptr_fields rest
   | [] -> []

let rec safe_args args visitor =
   let safe_arg (name, t, attr) =
      (safe_cpp name, visitCilType visitor t, attr) in
   match args with
   | arg :: rest -> safe_arg arg :: safe_args rest visitor
   | [] -> []

class safeCppTypes = object(self)
   inherit nopCilVisitor
   method vtype (t : typ) = 
      let self_visitor = (self :> cilVisitor) in
      match t with
      | TFun (ret, Some args, vararg, attr) ->
         let ret = visitCilType self_visitor ret in
         let args = safe_args args self_visitor in
         ChangeTo (TFun (ret, Some args, vararg, attr))

      | TPtr (pt, attr) -> 
         let pt = visitCilType self_visitor pt in
         (match pt with 
         | TFun _ -> DoChildren
         | _ ->
            let KPtr(id, orig_t, kptr_t) = kptr_typ pt in
            ChangeTo kptr_t)

      | TNamed _ -> ChangeTo (unrollType t)

      | _ -> DoChildren
end
let safeTypeVisitor = new safeCppTypes

let rec safe_cfields cfields visitor =
   let safe_cfield fi =
      fi.fname <- safe_cpp fi.fname;
      fi.ftype <- visitCilType visitor fi.ftype;
      fi.fattr <- visitCilAttributes safeTypeVisitor fi.fattr;
      fi in
   match cfields with
   | fi :: rest -> safe_cfield fi :: safe_cfields rest visitor
   | [] -> []

class safeCppFields = object(self)
   inherit nopCilVisitor
   method vtype (t : typ) =
      let self_visitor = (self :> cilVisitor) in
      match t with
      | TComp (comp, attr) ->
         comp.cname <- safe_cpp comp.cname;
         comp.cfields <- safe_cfields comp.cfields self_visitor;
         comp.cattr <- visitCilAttributes safeTypeVisitor comp.cattr;
         ChangeTo (TComp (comp, attr))
      | _ -> ChangeTo (visitCilType safeTypeVisitor t)
end
let safeFieldVisitor = new safeCppFields

class safeCppGlobals = object(self)
   inherit nopCilVisitor
   method vglob (g : global) = 
      match g with
      | GCompTag (comp, loc) ->
         comp.cname <- safe_cpp comp.cname;
         comp.cfields <- safe_cfields comp.cfields safeFieldVisitor;
         comp.cattr <- visitCilAttributes safeTypeVisitor comp.cattr;
         ChangeTo [GCompTag (comp, loc)]
      | GType (t, loc) ->
         t.ttype <- visitCilType safeTypeVisitor t.ttype;
         ChangeTo [GType (t, loc)]
      | _ -> DoChildren
end

(* Print all kptr construction macros *)
let print_kptrs _ =
   Hashtbl.iter (fun k v -> 
      match v with 
      | KPtr (id, TVoid _, _) ->
         fprintf !out_type_list "MAKE_VKPTR(%d);\n" id
      | KPtr (id, orig_type, _) -> 
         let td_g = GType ({tname="_kptr_" ^ (string_of_int id) ^ "_t";
                              ttype=orig_type; treferenced=false},
                           {line=(-1); file=""; byte=(-1)}) in
         let t_str =
            Pretty.sprint 80 (printGlobal !printerForMaincil () td_g) in
         let t_str = Str.global_replace (Str.regexp_string "\n") " " t_str in
         fprintf !out_type_list "MAKE_KPTR(%d, %s);\n" id t_str
      ) kptr_table

let print_typeapi cil_file = 
   out_type_list := Tools.output_file "type_list.h";
   visitCilFileSameGlobals (new printEnums) cil_file;
   visitCilFile (new safeCppGlobals) cil_file;
   print_kptrs ();
   visitCilFileSameGlobals (new printTypeDefs) cil_file;
