open Cil
open Printf

open Tools

(* List of all known allocation functions *)
let is_alloc_fn f = match f with
   | "kmem_cache_alloc"
   | "kmem_cache_alloc_notrace"
   | "kmem_cache_zalloc"
   | "kmalloc"
   | "kmalloc_node"
   | "kzalloc" 
   | "kzalloc_node" -> true
   | f -> false


(* A callsite lists the dest variable, function variable, and file/line *)
type callsite = {
   dst_var : varinfo;
   fn_var : varinfo;
   loc : location;
   cache : string option
}

let cs_name (c : callsite) = c.fn_var.vname

let cs_loc (c : callsite) = loc_str c.loc

let exp_var e =
   match e with
   | Lval (Var v, off) -> Some v.vname
   | _ -> None

let ctype_str t =
   Pretty.sprint 80 (!printerForMaincil#pType None () t)

(* Find all allocation functions, build a list of the variables that the
   result is assigned to *)
class listInstVisitor = object(self)
   inherit nopCilVisitor
   val mutable var_ids = ([] : callsite list)
   method vinst (i : instr) =
      match i with
      (* Allocator function call with variable result *)
      | Call (Some (Var v, _), Lval (Var f, _), arg1 :: args, location) ->
         if (is_alloc_fn f.vname) then (
            var_ids <- {dst_var = v; fn_var = f; loc = location; cache = exp_var arg1} :: var_ids
         ); DoChildren

      (* Allocator function call with memory result *)
      | Call (Some l, Lval (Var f, _), arg1 :: args, location) ->
         if (is_alloc_fn f.vname) then (
            match exp_var arg1 with
            | Some cache ->
               eprintf "\"%s\", \"%s\"\n" cache (ctype_str (typeOf (Lval l)))
            | _ -> ()
            (*
            print_string (f.vname ^ " " ^ (loc_str location) ^ " ");
            print_endline (type_str (typeOf (Lval l)))
            *)
         ); DoChildren

      (* Other allocator function call *)
      | Call (None, Lval (Var f, _), args, location) ->
         if (is_alloc_fn f.vname) then (
            print_string "**";
            print_loc location
         ); DoChildren

      | Call _
      | Set _
      | Asm _ -> DoChildren

   method get_ids = var_ids
end


(* Given a list of variable ids, find all the ways those variables are cast *)
class useVisitor (var_ids : callsite list) = object(self)
   inherit nopCilVisitor
   val mutable used_ids = ([] : callsite list)
   method vinst (i : instr) =
      match i with
      | Set (lval, exp, loc) -> (
         match exp with
         | CastE (t, Lval (Var v, off)) -> 
           (try (
              let cs = List.find (fun x -> (x.dst_var.vid = v.vid)) var_ids in
              (match cs.cache with
               | Some cache -> 
                  eprintf "\"%s\", \"%s\"\n" cache (ctype_str t)
               | _ -> ());
              (*
              print_string ((cs_name cs) ^ " " ^ (cs_loc cs) ^ " " ^ cs.cache);
              print_endline (type_str t);
              *)
              used_ids <- cs :: used_ids
           ) with Not_found -> ());
           DoChildren
         | exp -> DoChildren
        )
      | i -> DoChildren

   method get_ids = used_ids
end

exception UnusedError;;

let rec print_uses uses =
   match uses with
   | [] -> ()
   | hd :: tl -> print_int hd.dst_var.vid; print_string " "; print_uses tl

let check_use id uses = 
   if (List.exists (fun x -> x.dst_var.vid = id.dst_var.vid) uses) then ()
   else ((*prerr_endline ("BALLS! " ^ (loc_str id.loc))*)())

let check_uses uses ids =
   List.iter (fun x -> check_use x uses) ids

(* Visit all except allocation functions, run aforementioned two visitors to
 extract information on how allocation function results are cast *)
class fnVisitor = object(self)
   inherit nopCilVisitor
   method vfunc (f : fundec) =
      if not (is_alloc_fn f.svar.vname) then (
         let visit = (new listInstVisitor) in
         let _ = visitCilFunction (visit :> cilVisitor) f in
         let use = (new useVisitor (visit#get_ids)) in
         ignore (visitCilFunction (use :> cilVisitor) f);
         check_uses use#get_ids visit#get_ids
         (* print_endline ("Visited " ^ f.svar.vname); *)
      ) else ((*print_endline ("Skipped " ^ f.svar.vname)*));
      SkipChildren
end

let print_all_allocs cil_file = 
   ignore (visitCilFileSameGlobals (new fnVisitor) cil_file);
   ()
